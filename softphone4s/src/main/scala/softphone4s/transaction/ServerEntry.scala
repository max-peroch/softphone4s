package softphone4s.transaction

import cats.effect.std.Queue
import com.comcast.ip4s.{IpAddress, SocketAddress}

private[transaction] final case class ServerEntry[F[_]](
    machine: ServerMachine,
    from: SocketAddress[IpAddress],
    notices: Queue[F, Option[ServerNotice]]
)
