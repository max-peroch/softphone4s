package softphone4s.transport

import pureconfig.ConfigReader

import scala.concurrent.duration.*

/** How outgoing TCP and TLS connections are made and kept (RFC 3261 §18).
  *
  * @param connectTimeout
  *   how long to wait for a connection to be made.
  * @param idleTimeout
  *   a connection with nothing sent or received for this long is closed; it is
  *   opened again when the next message needs it.
  * @param framing
  *   the largest message a connection may carry.
  * @param keepAlive
  *   CRLF pings on quiet connections; none when absent. Pongs to pings from the
  *   other side are always sent.
  * @param maxIncoming
  *   how many connections made to us are served at once; the others wait.
  */
final case class StreamConfig(
    connectTimeout: FiniteDuration = 5.seconds,
    idleTimeout: FiniteDuration = 5.minutes,
    framing: FramingLimits = FramingLimits(),
    keepAlive: Option[KeepAlive] = None,
    maxIncoming: Int = 256
) derives ConfigReader
