package softphone4s.transport

import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

/** Whether a TLS server asks the client for a certificate (mutual TLS). */
enum ClientAuthentication(val token: String) {

  /** Never asks. */
  case Off extends ClientAuthentication("off")

  /** Asks, and carries on without one. */
  case Request extends ClientAuthentication("request")

  /** Asks, and refuses a client that has none we trust. */
  case Require extends ClientAuthentication("require")
}

object ClientAuthentication {
  given ConfigReader[ClientAuthentication] = ConfigReader[String].emap(s =>
    values
      .find(_.token == s)
      .toRight(
        CannotConvert(
          s,
          "ClientAuthentication",
          "expected off, request or require"
        )
      )
  )
}
