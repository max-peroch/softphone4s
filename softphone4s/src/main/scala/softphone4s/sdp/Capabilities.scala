package softphone4s.sdp

import cats.data.NonEmptyList
import softphone4s.model.PayloadType

/** What we can do with an audio stream: the codecs, most wanted first, the
  * payload type we receive telephone events on (RFC 4733), if we do, and the
  * one we receive comfort noise on (RFC 3389), if we do.
  */
final case class Capabilities(
    codecs: NonEmptyList[Codec],
    telephoneEventType: Option[PayloadType],
    comfortNoiseType: Option[PayloadType] = None
) {

  /** The same, with `codec` first if we have it: the one a call is already in
    * is answered before any other, so that a change of session does not change
    * the codec.
    */
  def preferring(codec: Codec): Capabilities =
    if codecs.exists(_ == codec) then
      copy(codecs = NonEmptyList(codec, codecs.filterNot(_ == codec)))
    else this

  /** The same, but with `codec` as the only one, if we have it. */
  def only(codec: Codec): Capabilities =
    if codecs.exists(_ == codec) then copy(codecs = NonEmptyList.one(codec))
    else this

  /** The payload types an offer of ours uses: a codec with a static one keeps
    * it, the others are given 96 and up, skipping the one telephone events
    * have.
    */
  def offered: List[(PayloadType, Codec)] = {
    val taken = (codecs.toList.flatMap(_.staticPayloadType) ++
      telephoneEventType.toList).toSet
    val free = PayloadType.Dynamic.filterNot(taken)
    codecs.toList
      .foldLeft((free, List.empty[(PayloadType, Codec)])) {
        case ((available, offered), codec) =>
          codec.staticPayloadType match {
            case Some(payloadType) =>
              (available, (payloadType -> codec) :: offered)
            case None =>
              available match {
                case payloadType :: rest =>
                  (rest, (payloadType -> codec) :: offered)
                // Out of dynamic numbers: this codec cannot be offered.
                case Nil => (Nil, offered)
              }
          }
      }
      ._2
      .reverse
  }
}

object Capabilities {

  /** PCMU and PCMA, with telephone events on 101. */
  val Default: Capabilities =
    Capabilities(
      NonEmptyList.of(Codec.Pcmu, Codec.Pcma),
      Some(PayloadType.TelephoneEvent),
      Some(PayloadType.ComfortNoise)
    )
}
