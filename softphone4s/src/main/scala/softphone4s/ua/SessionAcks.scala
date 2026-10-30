package softphone4s.ua

import cats.effect.std.Supervisor
import cats.effect.syntax.all.*
import cats.effect.{Async, Deferred, Ref}
import cats.syntax.all.*
import com.comcast.ip4s.{IpAddress, SocketAddress}
import org.typelevel.log4cats.StructuredLogger
import softphone4s.codec.SdpCodec
import softphone4s.codec.SdpCodec.AudioOffer
import softphone4s.fsm.OkRetransmission
import softphone4s.model.{CSeqNumber, SipRequest, SipResponse}
import softphone4s.transaction.TransactionConfig

/** The 2xx responses we sent to a re-INVITE and are still waiting to see ACKed.
  * Each is repeated on the INVITE schedule until its ACK comes, and if we made
  * the offer in the 2xx (the re-INVITE had none) the ACK carries the answer.
  */
private[softphone4s] final class SessionAcks[F[_]: Async] private (
    pending: Ref[F, Map[CSeqNumber, SessionAcks.Pending[F]]],
    supervisor: Supervisor[F],
    config: TransactionConfig,
    transmit: (SipResponse, SocketAddress[IpAddress]) => F[Unit],
    retarget: AudioOffer => F[Unit],
    logger: StructuredLogger[F]
) {

  /** Answers `request` with `respond`, which returns the response and where it
    * went, then repeats the response until the ACK. `expectsAnswer` is set when
    * the response carries our offer.
    */
  def track(request: SipRequest, expectsAnswer: Boolean)(
      respond: F[(SipResponse, SocketAddress[IpAddress])]
  ): F[Unit] = {
    val cseq = request.headers.cseq.seq
    for {
      acked <- Deferred[F, Unit]
      // Registered before the response goes out, so an ACK cannot beat it.
      _ <- pending.update(
        _.updated(cseq, SessionAcks.Pending(acked, expectsAnswer))
      )
      (response, address) <- respond
      _                   <- supervisor.supervise(
        repeat(cseq, response, address, acked)
      )
    } yield ()
  }

  private def repeat(
      cseq: CSeqNumber,
      response: SipResponse,
      to: SocketAddress[IpAddress],
      acked: Deferred[F, Unit]
  ): F[Unit] = {
    val schedule = OkRetransmission
      .delays(config)
      .traverse_(delay => Async[F].sleep(delay) >> transmit(response, to))
    acked.get.race(schedule).flatMap {
      case Left(_)  => Async[F].unit
      case Right(_) =>
        pending.update(_ - cseq) >>
          logger.warn(Map("cseq" -> cseq.value.toString))(
            "No ACK for the 200 to a re-INVITE"
          )
    }
  }

  /** An ACK arrived: stop repeating, and use the answer it carries if we were
    * waiting for one.
    */
  def acked(ack: SipRequest): F[Unit] =
    pending
      .modify(m => (m - ack.headers.cseq.seq, m.get(ack.headers.cseq.seq)))
      .flatMap(
        _.traverse_ { entry =>
          entry.acked.complete(()) >>
            SdpCodec
              .parseOffer(ack.body)
              .filter(_.isAcceptable)
              .filter(_ => entry.expectsAnswer)
              .traverse_(retarget)
        }
      )
}

private[softphone4s] object SessionAcks {

  final case class Pending[F[_]](
      acked: Deferred[F, Unit],
      expectsAnswer: Boolean
  )

  def create[F[_]: Async](
      supervisor: Supervisor[F],
      config: TransactionConfig,
      transmit: (SipResponse, SocketAddress[IpAddress]) => F[Unit],
      retarget: AudioOffer => F[Unit],
      logger: StructuredLogger[F]
  ): F[SessionAcks[F]] =
    Ref
      .of[F, Map[CSeqNumber, Pending[F]]](Map.empty)
      .map(new SessionAcks(_, supervisor, config, transmit, retarget, logger))
}
