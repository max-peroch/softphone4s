package softphone4s.model

import cats.{Eq, Show}

/** The transport protocols a SIP URI can name with `;transport=`. */
enum TransportProtocol(val token: String) {
  case Udp  extends TransportProtocol("udp")
  case Tcp  extends TransportProtocol("tcp")
  case Tls  extends TransportProtocol("tls")
  case Sctp extends TransportProtocol("sctp")
  case Ws   extends TransportProtocol("ws")
  case Wss  extends TransportProtocol("wss")
}

object TransportProtocol {
  def fromToken(s: String): Option[TransportProtocol] =
    values.find(_.token.equalsIgnoreCase(s))

  given Eq[TransportProtocol]   = Eq.fromUniversalEquals
  given Show[TransportProtocol] = Show.show(_.token)
}
