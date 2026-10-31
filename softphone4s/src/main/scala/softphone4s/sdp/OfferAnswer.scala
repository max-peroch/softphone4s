package softphone4s.sdp

import cats.syntax.all.*
import com.comcast.ip4s.{SocketAddress, port}
import softphone4s.model.MediaDirection

/** The offer/answer model of RFC 3264, as pure functions: the offer we make,
  * the answer we give to one, and what we make of an answer to ours. One audio
  * stream is used; any other is declined.
  */
object OfferAnswer {

  private val TelephoneEvent = "telephone-event"
  private val Events         = "0-16"

  /** An offer of all our codecs, in order of preference. */
  def offer(
      local: LocalMedia,
      capabilities: Capabilities,
      direction: MediaDirection
  ): SessionDescription = {
    val codecs = capabilities.offered
    SessionDescription(
      local.origin,
      "-",
      Some(local.address),
      Nil,
      List(
        MediaDescription(
          MediaKind.Audio,
          local.port,
          MediaProtocol.RtpAvp,
          (codecs.map(_._1) ++ capabilities.telephoneEventType)
            .map(_.toString),
          None,
          codecs.flatMap((pt, codec) => describe(pt, codec)) :::
            capabilities.telephoneEventType.toList.flatMap(telephoneEvents) :::
            List(Attribute.flag(direction.sdpAttribute))
        )
      )
    )
  }

  /** The answer to `offered`: the first of our codecs it offers, telephone
    * events if it offers them on the payload type we receive them on, and the
    * direction that goes with theirs. A stream we do not use is declined, in
    * its place.
    */
  def answer(
      offered: SessionDescription,
      local: LocalMedia,
      capabilities: Capabilities
  ): Either[Reject, Answered] =
    for {
      section <- offered.media
        .find(usable)
        .toRight(Reject.NoAudio)
      address <- offered.addressOf(section).toRight(Reject.NoAddress)
      meanings = section.meanings
      (codec, meaning) <- capabilities.codecs.toList
        .collectFirstSome(c => meanings.find(_.is(c)).map(c -> _))
        .toRight(Reject.NoCommonCodec)
      events = capabilities.telephoneEventType.filter(
        section.offersTelephoneEvents
      )
      theirs = offered.directionOf(section)
      ours   = theirs.answer
    } yield {
      val chosen = MediaDescription(
        MediaKind.Audio,
        local.port,
        MediaProtocol.RtpAvp,
        (meaning.payloadType :: events.toList).map(_.toString),
        None,
        describe(meaning.payloadType, codec) :::
          events.toList.flatMap(telephoneEvents) :::
          List(Attribute.flag(ours.sdpAttribute))
      )
      val sections = offered.media.map(m =>
        if m eq section then chosen
        else m.copy(port = port"0", connection = None, attributes = Nil)
      )
      Answered(
        SessionDescription(
          local.origin,
          "-",
          Some(local.address),
          Nil,
          sections
        ),
        MediaPlan(
          SocketAddress(address, section.port),
          codec,
          meaning.payloadType,
          events,
          theirs,
          ours
        )
      )
    }

  /** What `answered` agrees to, given the offer we made with `capabilities` and
    * `direction`: the first payload type in it that we offered, since the
    * answerer's order is its preference.
    */
  def accept(
      answered: SessionDescription,
      capabilities: Capabilities,
      direction: MediaDirection
  ): Either[Reject, MediaPlan] =
    for {
      section <- answered.media.find(usable).toRight(Reject.NoAudio)
      address <- answered.addressOf(section).toRight(Reject.NoAddress)
      ours = capabilities.offered.toMap
      maps = section.rtpMaps.map(m => m.payloadType -> m).toMap
      (payloadType, codec) <- section.payloadTypes
        .collectFirstSome(pt =>
          ours
            .get(pt)
            .filter(c => maps.get(pt).forall(_.is(c)))
            .map(pt -> _)
        )
        .toRight(Reject.NoCommonCodec)
    } yield MediaPlan(
      SocketAddress(address, section.port),
      codec,
      payloadType,
      capabilities.telephoneEventType.filter(section.offersTelephoneEvents),
      answered.directionOf(section),
      direction
    )

  /** An audio stream that is not declined and is plain RTP. */
  private def usable(section: MediaDescription): Boolean =
    section.kind == MediaKind.Audio && !section.isDeclined &&
      section.protocol == MediaProtocol.RtpAvp

  private def describe(payloadType: Int, codec: Codec): List[Attribute] =
    RtpMap(
      payloadType,
      codec.encoding,
      codec.clockRate,
      Option.when(codec.channels != 1)(codec.channels)
    ).attribute ::
      codec.fmtp.toList.map(f => Attribute.valued("fmtp", s"$payloadType $f"))

  private def telephoneEvents(payloadType: Int): List[Attribute] =
    List(
      RtpMap(payloadType, TelephoneEvent, 8000, None).attribute,
      Attribute.valued("fmtp", s"$payloadType $Events")
    )
}
