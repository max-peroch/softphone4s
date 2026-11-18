package softphone4s.transaction

import cats.effect.Deferred
import com.comcast.ip4s.{IpAddress, SocketAddress}
import softphone4s.model.*
import softphone4s.transport.Inbound

/** What the owner fiber of the transaction layer reacts to. */
private[transaction] sealed trait Command[F[_]]

private[transaction] object Command {
  final case class Receive[F[_]](inbound: Inbound) extends Command[F]

  final case class StartClient[F[_]](
      request: SipRequest,
      to: SocketAddress[IpAddress],
      reply: Deferred[F, Either[TransactionError, ClientTransaction[F]]]
  ) extends Command[F]

  final case class Respond[F[_]](key: ServerKey, response: SipResponse)
      extends Command[F]

  final case class Fired[F[_]](timer: TimerKey) extends Command[F]

  final case class SendFailed[F[_]](transaction: TransactionId)
      extends Command[F]
}
