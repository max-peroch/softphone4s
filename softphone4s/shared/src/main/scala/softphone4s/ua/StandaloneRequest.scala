package softphone4s.ua

import cats.effect.Async
import cats.syntax.all.*
import softphone4s.fsm.SipMessages.authorizationFor
import softphone4s.model.*
import softphone4s.runtime.Entropy
import softphone4s.transaction.{ClientNotice, TransactionLayer}

/** A request outside any dialog, sent once to a peer and answered: the pattern
  * of MESSAGE, and of the first SUBSCRIBE and PUBLISH. It gives the final
  * response, and if that is a challenge we can answer, answers it once and
  * gives the response to that.
  */
private[softphone4s] object StandaloneRequest {

  /** What became of a request. */
  enum Outcome {

    /** The final response, 200 to 699. */
    case Final(response: SipResponse)

    /** No final response in time. */
    case TimedOut

    /** It could not be sent, or the connection failed. */
    case TransportFailed
  }

  /** Sends what `build` makes, to the uplink's peer. `build` is given the
    * branch and CSeq of each try, and the Authorization of the second, so that
    * both carry the Call-ID and tags it holds; `body` is what `auth-int`
    * hashes. `nonceCount` gives the `nc` for a challenge: a request that shares
    * nonces with others must count them; one that does not gives
    * `NonceCount.One`.
    */
  def send[F[_]: Async](
      layer: TransactionLayer[F],
      entropy: Entropy[F],
      uplink: Uplink,
      method: SipMethod,
      target: SipUri,
      body: String,
      build: (Branch, CSeqNumber, Option[ExtensionHeader]) => SipRequest,
      nonceCount: DigestChallenge => F[NonceCount]
  ): F[Outcome] = {

    def attempt(
        cseq: CSeqNumber,
        authorization: Option[ExtensionHeader]
    ): F[Outcome] =
      entropy.branch.flatMap { branch =>
        layer.client(build(branch, cseq, authorization), uplink.peer).flatMap {
          case Left(_)            => Async[F].pure(Outcome.TransportFailed)
          case Right(transaction) =>
            transaction.notices
              .collectFirst {
                case ClientNotice.Response(r) if r.statusCode >= 200 =>
                  Outcome.Final(r)
                case ClientNotice.TimedOut        => Outcome.TimedOut
                case ClientNotice.TransportFailed => Outcome.TransportFailed
              }
              .compile
              .last
              .map(_.getOrElse(Outcome.TransportFailed))
        }
      }

    attempt(CSeqNumber.One, None).flatMap {
      case Outcome.Final(r @ Challenges.Challenged(kind, challenges)) =>
        Challenges
          .once(challenges, 0)
          .fold(Async[F].pure(Outcome.Final(r): Outcome))(challenge =>
            (entropy.cnonce, nonceCount(challenge)).flatMapN {
              (cnonce, count) =>
                attempt(
                  CSeqNumber.One.next,
                  Some(
                    uplink.endpoint.authorizationFor(
                      kind,
                      method,
                      target,
                      body,
                      challenge,
                      cnonce,
                      count
                    )
                  )
                )
            }
          )
      case other => Async[F].pure(other)
    }
  }
}
