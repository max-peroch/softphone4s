package softphone4s.config

import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

/** A codec that can be switched on, by the name it is configured with: one of
  * ours, or one the program has added, which is only known once it is looked
  * for.
  */
enum CodecChoice(val token: String) {

  /** G.711 μ-law. */
  case Pcmu extends CodecChoice("pcmu")

  /** G.711 A-law. */
  case Pcma extends CodecChoice("pcma")

  /** G.722, wideband: it needs an audio format of 16 kHz, so it cannot be mixed
    * with the G.711 codecs.
    */
  case G722 extends CodecChoice("g722")

  /** Any other name: a codec the program supplied, if it did. */
  case Named(name: String) extends CodecChoice(name)
}

object CodecChoice {
  given cats.kernel.Order[CodecChoice] = cats.kernel.Order.by(_.token)

  private val Ours = List(Pcmu, Pcma, G722)

  given ConfigReader[CodecChoice] = ConfigReader[String].emap { s =>
    val token = s.trim.toLowerCase
    Either.cond(
      token.nonEmpty,
      Ours.find(_.token == token).getOrElse(Named(token)),
      CannotConvert(s, "CodecChoice", "a codec needs a name")
    )
  }
}
