package softphone4s.media

import cats.data.NonEmptyList
import softphone4s.config.CodecChoice
import softphone4s.sdp.{Capabilities, Codec}

/** The codecs in use, most wanted first: what we offer, and what a negotiated
  * codec is played and recorded with. Further codecs can be added by building
  * one from their own [[AudioCodec]]s.
  */
final case class AudioCodecs(
    codecs: NonEmptyList[AudioCodec],
    telephoneEventType: Option[Int]
) {

  /** What offers and answers are made from. */
  def capabilities: Capabilities =
    Capabilities(codecs.map(_.description), telephoneEventType)

  /** The implementation of what a negotiation settled on. */
  def implementing(codec: Codec): Option[AudioCodec] =
    codecs.find(_.description == codec)

  /** The sample rate the audio device has to run at, if the codecs agree on
    * one; a mixture cannot be played without conversion.
    */
  def pcmRate: Option[Int] =
    Some(codecs.head.pcmRate).filter(r => codecs.forall(_.pcmRate == r))
}

object AudioCodecs {

  private def implementing(choice: CodecChoice): AudioCodec = choice match {
    case CodecChoice.Pcmu => G711.Pcmu
    case CodecChoice.Pcma => G711.Pcma
    case CodecChoice.G722 => G722.Wideband
  }

  /** The codecs chosen, in the order given, with telephone events on 101. */
  def of(choices: NonEmptyList[CodecChoice]): AudioCodecs =
    AudioCodecs(choices.distinct.map(implementing), Some(101))

  /** μ-law and A-law, with telephone events on 101. */
  val Default: AudioCodecs =
    AudioCodecs(NonEmptyList.of(G711.Pcmu, G711.Pcma), Some(101))
}
