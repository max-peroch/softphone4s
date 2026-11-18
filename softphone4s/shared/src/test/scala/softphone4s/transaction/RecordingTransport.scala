package softphone4s.transaction

import cats.effect.{Clock, Ref, Temporal}
import cats.syntax.all.*
import com.comcast.ip4s.{IpAddress, SocketAddress}
import fs2.Stream
import softphone4s.model.SipMessage
import softphone4s.transport.{Inbound, Transport}

import scala.concurrent.duration.FiniteDuration

/** A transport that remembers what was sent and when, and can lose messages.
  * `drop` is given how many messages were sent before this one (counting lost
  * ones) and the message itself.
  */
final class RecordingTransport[F[_]: Temporal] private (
    inner: Transport[F],
    sent: Ref[F, Vector[(FiniteDuration, SipMessage)]],
    drop: (Int, SipMessage) => Boolean
) extends Transport[F] {

  def localAddress: SocketAddress[IpAddress] = inner.localAddress
  def messages: Stream[F, Inbound]           = inner.messages

  def send(to: SocketAddress[IpAddress], message: SipMessage): F[Unit] =
    for {
      now   <- Clock[F].monotonic
      index <- sent.modify(v => (v :+ (now -> message), v.size))
      _     <-
        if drop(index, message) then Temporal[F].unit
        else inner.send(to, message)
    } yield ()

  /** Everything sent so far, with the time of each. */
  def log: F[Vector[(FiniteDuration, SipMessage)]] = sent.get
}

object RecordingTransport {

  def apply[F[_]: Temporal](
      inner: Transport[F],
      drop: (Int, SipMessage) => Boolean = (_, _) => false
  ): F[RecordingTransport[F]] =
    Ref
      .of[F, Vector[(FiniteDuration, SipMessage)]](Vector.empty)
      .map(
        new RecordingTransport(inner, _, drop)
      )
}
