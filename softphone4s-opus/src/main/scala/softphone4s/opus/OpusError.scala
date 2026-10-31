package softphone4s.opus

/** Why Opus cannot be used. */
sealed abstract class OpusError(message: String) extends Exception(message)

object OpusError {

  /** No libopus could be loaded, from any of the places that were tried. */
  final case class LibraryNotFound(tried: List[String])
      extends OpusError(
        s"Cannot load libopus (tried ${tried.mkString(", ")}); install it, or give its path in the settings"
      )

  /** A libopus that lacks a function we need, or that cannot be called. */
  final case class LibraryUnusable(reason: String)
      extends OpusError(s"libopus cannot be used: $reason")

  /** Settings that Opus does not allow. */
  final case class InvalidSettings(reason: String)
      extends OpusError(s"Invalid Opus settings: $reason")

  /** A libopus function that failed; `code` is its negative result. */
  final case class Native(function: String, code: Int)
      extends OpusError(s"libopus $function failed: ${describe(code)}")

  private def describe(code: Int): String = code match {
    case -1 => "bad argument"
    case -2 => "buffer too small"
    case -3 => "internal error"
    case -4 => "invalid packet"
    case -5 => "unimplemented"
    case -6 => "invalid state"
    case -7 => "allocation failed"
    case _  => s"error $code"
  }
}
