package softphone4s.transport

import pureconfig.ConfigReader

import scala.concurrent.duration.*

/** How outgoing TCP connections are made and kept (RFC 3261 §18).
  *
  * @param connectTimeout
  *   how long to wait for a connection to be made.
  * @param idleTimeout
  *   a connection with nothing sent or received for this long is closed; it is
  *   opened again when the next message needs it.
  * @param framing
  *   the largest message a connection may carry.
  */
final case class TcpConfig(
    connectTimeout: FiniteDuration = 5.seconds,
    idleTimeout: FiniteDuration = 5.minutes,
    framing: FramingLimits = FramingLimits()
) derives ConfigReader
