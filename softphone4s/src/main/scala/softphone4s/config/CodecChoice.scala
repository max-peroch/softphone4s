package softphone4s.config

import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

/** A codec that can be switched on, by the name it is configured with. */
enum CodecChoice(val token: String) {

  /** G.711 μ-law. */
  case Pcmu extends CodecChoice("pcmu")

  /** G.711 A-law. */
  case Pcma extends CodecChoice("pcma")

  /** G.722, wideband: it needs an audio format of 16 kHz, so it cannot be mixed
    * with the G.711 codecs.
    */
  case G722 extends CodecChoice("g722")
}

object CodecChoice {
  given cats.kernel.Order[CodecChoice] = cats.kernel.Order.by(_.ordinal)

  given ConfigReader[CodecChoice] = ConfigReader[String].emap(s =>
    values
      .find(_.token == s.toLowerCase)
      .toRight(
        CannotConvert(
          s,
          "CodecChoice",
          s"expected one of ${values.map(_.token).mkString(", ")}"
        )
      )
  )
}
