package softphone4s

import cats.data.NonEmptyList
import cats.effect.*
import cats.effect.std.{Queue, Supervisor}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import fs2.Stream
import fs2.concurrent.Channel
import fs2.io.net.Network
import org.typelevel.log4cats.StructuredLogger
import softphone4s.config.SoftphoneConfig
import softphone4s.fsm.CallTimer
import softphone4s.model.*
import softphone4s.media.AudioCodecs
import softphone4s.runtime.{Entropy, Timers}
import softphone4s.transaction.{
  ServerTransaction,
  TransactionConfig,
  TransactionLayer,
  TransactionMessages
}
import softphone4s.transport.Inbound
import softphone4s.fsm.SipMessages
import softphone4s.messaging.{IncomingMessage, MessageContent}
import softphone4s.ua.{DialogInput, InboundCall, MediaSession}

/** What arrives from the transaction layer, sent where it belongs: a request of
  * a call in progress to that call, a new INVITE to a new call (offered to the
  * program through `calls`), an instant message to `incomingMessages`, and the
  * rest answered with the status that says why not.
  */
private[softphone4s] final class IncomingRouter[F[_]: Async: Network](
    config: SoftphoneConfig,
    accountList: NonEmptyList[Account],
    layer: TransactionLayer[F],
    routes: Ref[F, Map[CallId, Channel[F, DialogInput[F]]]],
    endpoints: EndpointFactory[F],
    uplinks: Uplinks[F],
    entropy: Entropy[F],
    codecs: AudioCodecs,
    supervisor: Supervisor[F],
    incomingCalls: Queue[F, IncomingCall[F]],
    messageQueue: Queue[F, IncomingMessage],
    logger: StructuredLogger[F]
) {

  /** The account an incoming request is for: the one whose user is in the
    * Request-URI. With a single account every call is for it, whatever it
    * names.
    */
  private def accountFor(request: SipRequest): Option[Account] =
    if accountList.tail.isEmpty then Some(accountList.head)
    else
      request.requestUri.user.flatMap(user =>
        accountList.find(_.config.user == user.value)
      )

  def calls: Stream[F, IncomingCall[F]] =
    Stream.fromQueueUnterminated(incomingCalls)

  /** Hands each request to the call it belongs to. An INVITE with no To-tag and
    * a new Call-ID starts an incoming call; anything else for a call that is
    * gone, or never was, gets `481 Call/Transaction Does Not Exist` (RFC 3261
    * §12.2.2), and a new INVITE reusing a live Call-ID gets
    * `482 Loop Detected`.
    */
  private[softphone4s] def routeIncoming: F[Unit] =
    layer.incoming.evalMap(route).compile.drain

  private def isNewInvite(request: SipRequest): Boolean =
    request.method == SipMethod.Invite && request.headers.to.tag.isEmpty

  private def route(transaction: ServerTransaction[F]): F[Unit] = {
    val request = transaction.request
    routes.get.flatMap(_.get(request.headers.callId) match {
      case Some(_) if isNewInvite(request) =>
        transaction.respond(
          TransactionMessages.responseTo(request, 482, "Loop Detected")
        )
      case Some(channel) => channel.send(DialogInput.Request(transaction)).void
      case None if isNewInvite(request) =>
        accountFor(request).fold(
          transaction.respond(
            TransactionMessages.responseTo(
              request,
              FailureStatus.NotFound.value,
              FailureStatus.NotFound.reason
            )
          )
        )(acceptInbound(transaction, _))
      case None if request.method == SipMethod.Message =>
        acceptMessage(transaction)
      case None =>
        logger.debug(Map("callId" -> request.headers.callId.value))(
          "Request for a call that does not exist"
        ) >>
          transaction.respond(
            TransactionMessages.responseTo(
              request,
              FailureStatus.CallDoesNotExist.value,
              FailureStatus.CallDoesNotExist.reason
            )
          )
    })
  }

  def incomingMessages: Stream[F, IncomingMessage] =
    Stream.fromQueueUnterminated(messageQueue)

  /** A MESSAGE outside any dialog (RFC 3428). It is answered 404 if no account
    * is the one it is for, 400 if it has a body and does not say what it is,
    * 415 if it is not text we can read, and 503 if nobody is reading and the
    * queue is full; otherwise it is held for `incomingMessages` and answered
    * 200, in that order, so that 200 means the program has it.
    */
  private def acceptMessage(transaction: ServerTransaction[F]): F[Unit] = {
    val request                       = transaction.request
    def refuse(status: FailureStatus) =
      transaction.respond(
        TransactionMessages.responseTo(request, status.value, status.reason)
      )
    accountFor(request) match {
      case None          => refuse(FailureStatus.NotFound)
      case Some(account) =>
        request.headers.contentType match {
          case None if request.body.nonEmpty =>
            refuse(FailureStatus.BadRequest)
          case Some(contentType) if !MessageContent.canRead(contentType) =>
            transaction.respond(
              SipMessages.unsupportedMessage(request, MessageContent.Accepted)
            )
          case contentType =>
            val message = IncomingMessage(
              account.id,
              request.headers.from,
              MessageContent(
                contentType.getOrElse(MediaType.Plain),
                request.body
              )
            )
            messageQueue.tryOffer(message).flatMap {
              case true  => transaction.respond(SipMessages.messageOk(request))
              case false =>
                logger.warn(Map("account" -> account.id.render))(
                  "A message was refused: nobody is reading them"
                ) >> refuse(FailureStatus.ServiceUnavailable)
            }
        }
    }
  }

  /** Hands messages no transaction claimed (the ACK of a 2xx, a retransmitted
    * 2xx) to their call.
    */
  private[softphone4s] def routeUnmatched: F[Unit] =
    layer.unmatched
      .evalMap { (inbound: Inbound) =>
        routes.get.flatMap(_.get(inbound.message.headers.callId) match {
          case Some(channel) =>
            channel.send(DialogInput.Unmatched(inbound)).void
          case None =>
            logger.debug(Map("callId" -> inbound.message.headers.callId.value))(
              "Dropping a message for a call that does not exist"
            )
        })
      }
      .compile
      .drain

  /** Registers the route first, so a CANCEL or ACK right behind the INVITE
    * finds its call, then runs the call on its own fiber for as long as it
    * lasts.
    */
  private def acceptInbound(
      invite: ServerTransaction[F],
      account: Account
  ): F[Unit] = {
    val callId = invite.request.headers.callId
    Channel.bounded[F, DialogInput[F]](128).flatMap { channel =>
      // Registered and handed to a fiber that unregisters it, or neither.
      Async[F].uncancelable(_ =>
        routes.update(_ + (callId -> channel)) >>
          supervisor
            .supervise(
              inboundCall(invite, account, channel)
                .handleErrorWith { error =>
                  logger
                    .error(Map("error" -> String.valueOf(error.getMessage)))(
                      "Could not take the incoming call"
                    ) >> invite.respond(
                    TransactionMessages.responseTo(
                      invite.request,
                      FailureStatus.ServerInternalError.value,
                      FailureStatus.ServerInternalError.reason
                    )
                  )
                }
                .guarantee(routes.update(_ - callId))
            )
            .void
            .onError(_ => routes.update(_ - callId))
      )
    }
  }

  /** Builds the call's resources, offers the call to the application if it can
    * be answered, and runs it to its end.
    */
  private def inboundCall(
      invite: ServerTransaction[F],
      account: Account,
      channel: Channel[F, DialogInput[F]]
  ): F[Unit] =
    (for {
      callSupervisor <- Supervisor[F](await = false)
      timers         <- Timers.resource[F, CallTimer]
      localIp        <- Resource.eval(endpoints.localIpFor(invite.from))
      endpoint       <- endpoints
        .allocateEndpoint(
          account,
          localIp,
          invite.request.headers.topVia.transport
        )
        .evalMap(endpoints.secureIfNeeded(_, invite.request.requestUri.scheme))
        .evalMap(endpoints.seenFromOutside)
      media <- Resource.eval(
        MediaSession
          .create(
            endpoint.bindAddress,
            endpoint.rtpPort,
            codecs,
            config.nat.rtpLatching,
            config.jitterBuffer
          )
      )
      call <- Resource.eval(
        InboundCall.create(
          invite,
          account.id,
          layer,
          (transport, uri) => uplinks.nextHopOver(transport)(uri),
          config.maxUdpMessage,
          endpoint,
          callSupervisor,
          entropy,
          media,
          timers,
          TransactionConfig(),
          config.answerTimeout,
          config.hangupTimeout,
          logger
        )
      )
    } yield call).use { call =>
      call.isOpen.flatMap(incomingCalls.offer(call).whenA(_)) >>
        call.run(channel.stream)
    }
}
