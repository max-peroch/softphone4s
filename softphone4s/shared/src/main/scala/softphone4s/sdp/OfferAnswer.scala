package softphone4s.sdp

import cats.syntax.all.*
import com.comcast.ip4s.{SocketAddress, port}
import softphone4s.model.{MediaDirection, PayloadType}
import softphone4s.srtp.{SrtpPlan, SrtpPolicy}

/** The offer/answer model of RFC 3264, as pure functions: the offer we make,
  * the answer we give to one, and what we make of an answer to ours. One audio
  * stream is used; any other is declined.
  */
object OfferAnswer {

  private val TelephoneEvent = "telephone-event"
  private val Events         = "0-16"

  /** An offer of all our codecs, in order of preference. With keys in
    * `capabilities` it carries our keys: as `RTP/SAVP` if encryption is
    * required, and as `RTP/AVP` with the keys alongside if it is optional, so
    * that the other side may choose (RFC 4568 §3.1).
    */
  def offer(
      local: LocalMedia,
      capabilities: Capabilities,
      direction: MediaDirection
  ): SessionDescription = {
    val codecs = capabilities.offered
    val crypto = capabilities.crypto
    SessionDescription(
      local.origin,
      "-",
      Some(local.address),
      Nil,
      List(
        MediaDescription(
          MediaKind.Audio,
          local.port,
          if crypto.policy == SrtpPolicy.Required then MediaProtocol.RtpSavp
          else MediaProtocol.RtpAvp,
          (codecs.map(_._1) ++ capabilities.telephoneEventType ++
            capabilities.comfortNoiseType).map(_.toString),
          None,
          codecs.flatMap((pt, codec) => describe(pt, codec)) :::
            capabilities.telephoneEventType.toList.flatMap(telephoneEvents) :::
            capabilities.comfortNoiseType.toList.map(comfortNoise) :::
            (if crypto.policy == SrtpPolicy.Off then Nil
             else crypto.lines.map(_.attribute)) :::
            List(Attribute.flag(direction.sdpAttribute))
        )
      )
    )
  }

  /** The answer to `offered`: the first of our codecs it offers, telephone
    * events if it offers them on the payload type we receive them on, and the
    * direction that goes with theirs. A stream we do not use is declined, in
    * its place.
    *
    * With keys in `capabilities`, the first of the keys they offer in a suite
    * we speak is taken, and answered with ours under the same number, in the
    * profile they used. Without a key we can use, the call is plain if they
    * offered plain RTP and we do not require encryption, and is refused
    * otherwise.
    */
  def answer(
      offered: SessionDescription,
      local: LocalMedia,
      capabilities: Capabilities
  ): Either[Reject, Answered] =
    for {
      section <- offered.media
        .find(usable(_, capabilities.crypto.policy))
        .toRight(Reject.NoAudio)
      address <- offered.addressOf(section).toRight(Reject.NoAddress)
      meanings = section.meanings
      (codec, meaning) <- capabilities.codecs.toList
        .collectFirstSome(c => meanings.find(_.is(c)).map(c -> _))
        .toRight(Reject.NoCommonCodec)
      events = capabilities.telephoneEventType.filter(
        section.offersTelephoneEvents
      )
      comfort = capabilities.comfortNoiseType.flatMap(_ =>
        section.comfortNoiseType
      )
      keyed <- chooseKeys(section, capabilities.crypto, bySuite)
      theirs = offered.directionOf(section)
      ours   = theirs.answer
    } yield {
      val chosen = MediaDescription(
        MediaKind.Audio,
        local.port,
        section.protocol,
        (meaning.payloadType :: events.toList ::: comfort.toList)
          .map(_.toString),
        None,
        describe(meaning.payloadType, codec) :::
          events.toList.flatMap(telephoneEvents) :::
          comfort.toList.map(comfortNoise) :::
          keyed.toList.map((theirKey, ourKey) =>
            ourKey.copy(tag = theirKey.tag).attribute
          ) :::
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
          ours,
          comfort,
          keyed.map((theirKey, ourKey) =>
            SrtpPlan(ourKey.suite, ourKey.master, theirKey.master)
          )
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
      section <- answered.media
        .find(usable(_, capabilities.crypto.policy))
        .toRight(Reject.NoAudio)
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
      // They answer with one of the numbers we offered, and its suite.
      keyed <- chooseKeys(section, capabilities.crypto, byTag)
    } yield MediaPlan(
      SocketAddress(address, section.port),
      codec,
      payloadType,
      capabilities.telephoneEventType.filter(section.offersTelephoneEvents),
      answered.directionOf(section),
      direction,
      capabilities.comfortNoiseType.flatMap(_ => section.comfortNoiseType),
      keyed.map((theirKey, ourKey) =>
        SrtpPlan(ourKey.suite, ourKey.master, theirKey.master)
      )
    )

  /** An audio stream that is not declined and is RTP, secured only if we
    * encrypt at all.
    */
  private def usable(section: MediaDescription, policy: SrtpPolicy): Boolean =
    section.kind == MediaKind.Audio && !section.isDeclined &&
      (section.protocol match {
        case MediaProtocol.RtpAvp  => true
        case MediaProtocol.RtpSavp => policy != SrtpPolicy.Off
        case _                     => false
      })

  /** A key of theirs and the key of ours it is paired with. */
  private type Keys = Option[(CryptoAttribute, CryptoAttribute)]

  /** For an answer: the first key of theirs in a suite we speak, with ours for
    * that suite.
    */
  private def bySuite(
      theirs: List[CryptoAttribute],
      ours: List[CryptoAttribute]
  ): Keys =
    theirs.collectFirstSome(t => ours.find(_.suite == t.suite).map(t -> _))

  /** For accepting an answer: the key of theirs that answers one of ours, by
    * its number and suite.
    */
  private def byTag(
      theirs: List[CryptoAttribute],
      ours: List[CryptoAttribute]
  ): Keys =
    theirs.collectFirstSome(t =>
      ours.find(o => o.tag == t.tag && o.suite == t.suite).map(t -> _)
    )

  /** The keys to encrypt `section` with, as `pick` finds them. None is plain,
    * which is allowed for plain RTP when encryption is not required; media
    * offered as secure with no key we can use, and any media when encryption is
    * required, is refused.
    */
  private def chooseKeys(
      section: MediaDescription,
      crypto: LocalCrypto,
      pick: (List[CryptoAttribute], List[CryptoAttribute]) => Keys
  ): Either[Reject, Keys] =
    crypto.policy match {
      case SrtpPolicy.Off => Right(None)
      case policy         =>
        pick(section.cryptos, crypto.lines) match {
          case found @ Some(_) => Right(found)
          case None            =>
            Either.cond(
              policy != SrtpPolicy.Required &&
                section.protocol == MediaProtocol.RtpAvp,
              None,
              Reject.NoCommonCrypto
            )
        }
    }

  private def describe(
      payloadType: PayloadType,
      codec: Codec
  ): List[Attribute] =
    RtpMap(
      payloadType,
      codec.encoding,
      codec.clockRate,
      Option.when(codec.channels != 1)(codec.channels)
    ).attribute ::
      codec.fmtp.toList.map(f => Attribute.valued("fmtp", s"$payloadType $f"))

  private def comfortNoise(payloadType: PayloadType): Attribute =
    RtpMap(payloadType, "CN", 8000, None).attribute

  private def telephoneEvents(payloadType: PayloadType): List[Attribute] =
    List(
      RtpMap(payloadType, TelephoneEvent, 8000, None).attribute,
      Attribute.valued("fmtp", s"$payloadType $Events")
    )
}
