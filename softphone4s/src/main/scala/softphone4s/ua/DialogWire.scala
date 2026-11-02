package softphone4s.ua

import cats.effect.Async
import cats.effect.std.Supervisor
import cats.effect.Ref
import cats.syntax.all.*
import com.comcast.ip4s.{IpAddress, SocketAddress}
import org.typelevel.log4cats.StructuredLogger
import softphone4s.fsm.{StreamSwitch, TransactionFailure}
import softphone4s.model.*
import softphone4s.transaction.{
  ClientNotice,
  ClientTransaction,
  ServerTransaction,
  TransactionLayer
}

private[ua] object DialogWire {

  /** What a client transaction tells its call, which the call turns into an
    * event of its own.
    */
  enum Notice {
    case Response(response: SipResponse)
    case Failed(failure: TransactionFailure)
  }
}

/** How a call sends requests and answers them within its dialog, which is the
  * same for an outgoing call and an incoming one: the next hop, a request too
  * big for UDP going over TCP, a client transaction and what it reports, and
  * the answer to a request through its server transaction. The two differ in
  * where the current uplink comes from (an outgoing call may change it after a
  * failure), in the events they report to, and in what they fall back to when
  * the next hop cannot be found.
  *
  * @param current
  *   the uplink the call has now.
  * @param report
  *   what a call does with what a client transaction reports.
  * @param fallback
  *   names, for the log, what a request goes to when its next hop cannot be
  *   resolved: the current uplink's peer.
  */
private[ua] final class DialogWire[F[_]: Async](
    layer: TransactionLayer[F],
    resolve: (TransportProtocol, SipUri) => F[SocketAddress[IpAddress]],
    udpLimit: Int,
    supervisor: Supervisor[F],
    logger: StructuredLogger[F],
    newBranch: F[Branch],
    current: F[Uplink],
    serverTransactions: Ref[F, Map[(SipMethod, CSeqNumber), ServerTransaction[
      F
    ]]],
    acks: SessionAcks[F],
    report: DialogWire.Notice => F[Unit],
    fallback: String
) {

  /** Where a request within the dialog goes: the next hop of its route (RFC
    * 3261 §12.2.1.1), or the current uplink's peer if that cannot be resolved.
    */
  def destinationFor(dialog: SipDialog): F[SocketAddress[IpAddress]] =
    current.flatMap { uplink =>
      resolve(uplink.endpoint.transport, dialog.route.nextHop)
        .handleErrorWith { error =>
          logger
            .warn(Map("error" -> String.valueOf(error.getMessage)))(
              s"Cannot resolve the next hop; using $fallback"
            )
            .as(uplink.peer)
        }
    }

  /** Sends a request within `dialog` to `to`. One that would go over UDP but is
    * too big for a datagram goes over TCP instead (RFC 3261 §18.1.1): its Via
    * and Contact are made TCP's, it is sent to where the dialog's next hop
    * serves TCP, and if that connection cannot be made it is sent again over
    * UDP as a new transaction.
    */
  def startInDialog(
      request: SipRequest,
      dialog: SipDialog,
      to: SocketAddress[IpAddress]
  ): F[Unit] =
    current.flatMap { uplink =>
      uplink.endpoint.via(uplink.endpoint.ip, TransportProtocol.Tcp) match {
        case Right(tcp) if StreamSwitch.tooBigForUdp(request, udpLimit) =>
          for {
            tcpTo <- resolve(TransportProtocol.Tcp, dialog.route.nextHop)
              .handleError(_ => to)
            _ <- logger.info(Map("method" -> request.method.toString))(
              "Request too big for UDP; sending it over TCP"
            )
            _ <- startClient(
              StreamSwitch.over(request, tcp),
              tcpTo,
              failover = newBranch.flatMap(branch =>
                startClient(StreamSwitch.reissued(request, branch), to).as(true)
              )
            )
          } yield ()
        case _ => startClient(request, to)
      }
    }

  /** Sends a request that is not part of a transaction, the ACK of a 2xx,
    * within `dialog`. Too big for UDP, it goes over TCP, and over UDP after all
    * if that cannot be written.
    */
  def transmitInDialog(
      request: SipRequest,
      dialog: SipDialog,
      to: SocketAddress[IpAddress]
  ): F[Unit] =
    current.flatMap { uplink =>
      uplink.endpoint.via(uplink.endpoint.ip, TransportProtocol.Tcp) match {
        case Right(tcp) if StreamSwitch.tooBigForUdp(request, udpLimit) =>
          resolve(TransportProtocol.Tcp, dialog.route.nextHop)
            .handleError(_ => to)
            .flatMap(tcpTo =>
              layer
                .transmit(StreamSwitch.over(request, tcp), tcpTo)
                .handleErrorWith(_ => layer.transmit(request, to))
            )
        case _ => layer.transmit(request, to)
      }
    }

  /** Sends `request` in a client transaction and turns what the transaction
    * reports into events. A request that cannot even start counts as a
    * transport failure.
    */
  def startClient(
      request: SipRequest,
      to: SocketAddress[IpAddress],
      failover: F[Boolean] = Async[F].pure(false)
  ): F[Unit] =
    layer.client(request, to).flatMap {
      case Right(transaction) =>
        supervisor.supervise(forward(transaction, failover)).void
      case Left(error) =>
        logger.warn(Map("error" -> error.message))(
          "Cannot start a transaction"
        ) >> report(
          DialogWire.Notice.Failed(TransactionFailure.TransportFailed)
        )
    }

  /** Passes on what a client transaction reports. When it ends without an
    * answer, `failover` may find another way to try; if it does, that is the
    * end of this transaction and nothing is reported.
    */
  def forward(
      transaction: ClientTransaction[F],
      failover: F[Boolean]
  ): F[Unit] =
    transaction.notices
      .evalMap {
        case ClientNotice.Response(response) =>
          report(DialogWire.Notice.Response(response))
        case ClientNotice.TimedOut =>
          failover.ifM(
            Async[F].unit,
            report(DialogWire.Notice.Failed(TransactionFailure.TimedOut))
          )
        case ClientNotice.TransportFailed =>
          failover.ifM(
            Async[F].unit,
            report(DialogWire.Notice.Failed(TransactionFailure.TransportFailed))
          )
      }
      .compile
      .drain

  /** Answers the in-dialog request `request` through its server transaction. */
  def respondTo(request: SipRequest, response: SipResponse): F[Unit] =
    respondFrom(request, response).void

  /** As [[respondTo]], and says where the response went. */
  def respondFrom(
      request: SipRequest,
      response: SipResponse
  ): F[SocketAddress[IpAddress]] = {
    val key = (request.method, request.headers.cseq.seq)
    serverTransactions.modify(m => (m - key, m.get(key))).flatMap {
      case Some(transaction) =>
        transaction.respond(response).as(transaction.from)
      case None =>
        logger.warn(Map("method" -> request.method.toString))(
          "No server transaction to answer; sending the response directly"
        ) >> current.flatMap(uplink =>
          layer.transmit(response, uplink.peer).as(uplink.peer)
        )
    }
  }

  /** Answers a re-INVITE or UPDATE. The 2xx to a re-INVITE is repeated until
    * its ACK, which carries our answer if the 2xx carried our offer.
    */
  def answerSession(
      request: SipRequest,
      ok: SipResponse,
      ack: AckContent
  ): F[Unit] =
    if request.method == SipMethod.Invite then
      acks.track(request, ack)(
        respondFrom(request, ok).map(ok -> _)
      )
    else respondTo(request, ok)
}
