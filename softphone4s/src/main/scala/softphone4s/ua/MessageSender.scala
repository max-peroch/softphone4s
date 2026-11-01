package softphone4s.ua

import cats.effect.Async
import cats.syntax.all.*
import softphone4s.fsm.SipMessages.buildMessage
import softphone4s.messaging.{MessageContent, MessageFailure}
import softphone4s.model.{SipMethod, SipUri, Uplink}
import softphone4s.runtime.Entropy
import softphone4s.transaction.TransactionLayer

/** Delivers an instant message (RFC 3428): a MESSAGE to each of the ways to
  * reach the recipient in turn, until one answers.
  */
private[softphone4s] final class MessageSender[F[_]: Async](
    layer: TransactionLayer[F],
    entropy: Entropy[F]
) {

  /** Sends `content` to `recipient` over the first of `candidates`, and over
    * the next if that gets no answer or cannot be used. A refusal is final:
    * another route would get the same one. The candidates are made only when
    * their turn comes.
    */
  def deliver(
      candidates: List[F[Uplink]],
      recipient: SipUri,
      content: MessageContent
  ): F[Either[MessageFailure, Unit]] =
    candidates match {
      case Nil           => Async[F].pure(Left(MessageFailure.TransportFailed))
      case first :: rest =>
        first.flatMap(sendOver(_, recipient, content)).flatMap {
          case Left(MessageFailure.TimedOut | MessageFailure.TransportFailed)
              if rest.nonEmpty =>
            deliver(rest, recipient, content)
          case result => Async[F].pure(result)
        }
    }

  private def sendOver(
      uplink: Uplink,
      recipient: SipUri,
      content: MessageContent
  ): F[Either[MessageFailure, Unit]] =
    (entropy.callId(uplink.endpoint.ip), entropy.tag).flatMapN {
      (callId, localTag) =>
        StandaloneRequest
          .send(
            layer,
            entropy,
            uplink,
            SipMethod.Message,
            recipient,
            content.text,
            (branch, cseq, authorization) =>
              uplink.endpoint.buildMessage(
                recipient,
                callId,
                localTag,
                cseq,
                branch,
                content,
                authorization
              )
          )
          .map {
            case StandaloneRequest.Outcome.Final(response) =>
              MessageFailure.of(response)
            case StandaloneRequest.Outcome.TimedOut =>
              Left(MessageFailure.TimedOut)
            case StandaloneRequest.Outcome.TransportFailed =>
              Left(MessageFailure.TransportFailed)
          }
    }
}
