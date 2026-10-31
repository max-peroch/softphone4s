package softphone4s.media

import cats.data.NonEmptyList
import softphone4s.config.CodecChoice
import softphone4s.model.{PayloadType, SoftphoneError}
import softphone4s.sdp.{Capabilities, Codec}

/** The codecs in use, most wanted first: what we offer, and what a negotiated
  * codec is played and recorded with.
  */
final case class AudioCodecs(
    codecs: NonEmptyList[AudioCodec],
    telephoneEventType: Option[PayloadType]
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

  /** The codecs that come with the library. */
  val BuiltIn: List[AudioCodec] =
    List(G711.Pcmu, G711.Pcma, G722.Wideband)

  /** The codecs chosen, in the order given, found among ours and `extras`, with
    * telephone events on 101. A name that nothing has is an error, and so are
    * two codecs of one name.
    */
  def resolve(
      choices: NonEmptyList[CodecChoice],
      extras: List[AudioCodec]
  ): Either[SoftphoneError, AudioCodecs] = {
    val available = BuiltIn ++ extras
    for {
      _ <- available
        .groupBy(_.token)
        .collectFirst { case (token, same) if same.size > 1 => token }
        .fold(Right(()))(token => Left(SoftphoneError.DuplicateCodec(token)))
      found <- choices.distinct.traverse(choice =>
        available
          .find(_.token == choice.token)
          .toRight(SoftphoneError.UnknownCodec(choice.token))
      )
    } yield AudioCodecs(found, Some(PayloadType.TelephoneEvent))
  }

  /** μ-law and A-law, with telephone events on 101. */
  val Default: AudioCodecs =
    AudioCodecs(
      NonEmptyList.of(G711.Pcmu, G711.Pcma),
      Some(PayloadType.TelephoneEvent)
    )
}
