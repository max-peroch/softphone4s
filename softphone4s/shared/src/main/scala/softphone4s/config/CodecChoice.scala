package softphone4s.config

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

}
