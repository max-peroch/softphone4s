package softphone4s.sdp

import cats.data.NonEmptyList

/** What we can do with an audio stream: the codecs, most wanted first, and the
  * payload type we receive telephone events on (RFC 4733), if we do.
  */
final case class Capabilities(
    codecs: NonEmptyList[Codec],
    telephoneEventType: Option[Int]
) {

  /** The payload types an offer of ours uses: a codec with a static one keeps
    * it, the others are given 96 and up, skipping the one telephone events
    * have.
    */
  def offered: List[(Int, Codec)] = {
    val taken = (codecs.toList.flatMap(_.staticPayloadType) ++
      telephoneEventType.toList).toSet
    val dynamic = Iterator.from(96).filterNot(taken).take(codecs.size)
    codecs.toList.map(codec =>
      codec.staticPayloadType.getOrElse(dynamic.next()) -> codec
    )
  }
}

object Capabilities {

  /** PCMU, with telephone events on 101. */
  val Default: Capabilities =
    Capabilities(NonEmptyList.one(Codec.Pcmu), Some(101))
}
