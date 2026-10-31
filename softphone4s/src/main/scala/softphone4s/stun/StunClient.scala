package softphone4s.stun

import com.comcast.ip4s.{IpAddress, SocketAddress}

/** Asking a STUN server what address it sees us at, from the socket we send SIP
  * from, so that the answer is the mapping SIP traffic gets.
  */
trait StunClient[F[_]] {

  /** The address `server` saw a Binding request come from. The request is
    * repeated, waiting `retransmit` and then twice as long each time, up to
    * `attempts` times; the last wait is the same as the one before it.
    */
  def bindingRequest(
      server: SocketAddress[IpAddress],
      retransmit: scala.concurrent.duration.FiniteDuration,
      attempts: Int
  ): F[Either[Stun.Error, SocketAddress[IpAddress]]]
}
