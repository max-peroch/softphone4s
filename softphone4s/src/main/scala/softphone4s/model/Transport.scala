package softphone4s.model

import cats.{Eq, Show}

/** Transport protocols a SIP URI can name with `;transport=`. */
enum Transport(val token: String) {
  case Udp  extends Transport("udp")
  case Tcp  extends Transport("tcp")
  case Tls  extends Transport("tls")
  case Sctp extends Transport("sctp")
  case Ws   extends Transport("ws")
  case Wss  extends Transport("wss")
}

object Transport {
  def fromToken(s: String): Option[Transport] =
    values.find(_.token.equalsIgnoreCase(s))

  given Eq[Transport]   = Eq.fromUniversalEquals
  given Show[Transport] = Show.show(_.token)
}
