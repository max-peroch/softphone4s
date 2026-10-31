package softphone4s.codec

import com.comcast.ip4s.{IpAddress, Port, ip, port}
import softphone4s.model.MediaDirection
import softphone4s.sdp.{
  Capabilities,
  LocalMedia,
  MediaKind,
  MediaProtocol,
  OfferAnswer,
  SessionDescription
}

/** The SDP the call code deals in: one audio stream, PCMU and telephone events
  * on 101. It reads and writes through [[softphone4s.sdp]], which does the
  * parsing and the offer/answer; this is the shape the dialogs were written
  * against, until they take a negotiated plan instead.
  */
object SdpCodec {

  private val TelephoneEventPayloadType = 101

  /** A local address that stands for ours when only the outcome of an answer
    * matters.
    */
  private val probe = LocalMedia(ip"0.0.0.0", port"0", 0, 0)

  /** Extract the remote RTP endpoint from an SDP body. */
  def parseAudioEndpoint(
      sdp: String,
      defaultIp: IpAddress,
      defaultPort: Port
  ): (IpAddress, Port) =
    SessionDescription
      .parse(sdp)
      .toOption
      .flatMap(d => d.media.find(_.kind == MediaKind.Audio).map(m => (d, m)))
      .fold((defaultIp, defaultPort))((d, m) =>
        (d.addressOf(m).getOrElse(defaultIp), m.port)
      )

  /** What a caller's SDP offers for audio. */
  final case class AudioOffer(
      ip: IpAddress,
      port: Port,
      payloadTypes: List[Int],
      offersTelephoneEvent: Boolean,
      direction: MediaDirection,
      description: SessionDescription
  ) {

    /** Whether this offer puts the call on hold: nothing is to be sent to its
      * sender (`sendonly`, `inactive`, or the old connection address 0.0.0.0).
      */
    def isHold: Boolean = !direction.receives

    /** Whether we can answer it: a codec of ours is in it. */
    def isAcceptable: Boolean =
      OfferAnswer.answer(description, probe, Capabilities.Default).isRight
  }

  /** The audio an SDP body offers, or `None` if it has no audio stream we can
    * address: one that is not declined, is plain RTP and has an address.
    * Telephone-event counts only on payload type 101, the one the RTP code
    * receives.
    */
  def parseOffer(sdp: String): Option[AudioOffer] =
    for {
      description <- SessionDescription.parse(sdp).toOption
      section     <- description.media.find(m =>
        m.kind == MediaKind.Audio && !m.isDeclined &&
          m.protocol == MediaProtocol.RtpAvp
      )
      ip <- description.addressOf(section)
    } yield AudioOffer(
      ip,
      section.port,
      section.payloadTypes,
      section.offersTelephoneEvents(TelephoneEventPayloadType),
      description.directionOf(section),
      description
    )

  /** An answer to `offer`: PCMU, and telephone-event only if it was offered, so
    * the answer stays within what the caller proposed (RFC 3264 §6).
    */
  def buildAnswerTo(
      offer: AudioOffer,
      localIp: IpAddress,
      localRtpPort: Port
  ): String = {
    val local = LocalMedia(localIp, localRtpPort, 0, 0)
    OfferAnswer
      .answer(offer.description, local, Capabilities.Default)
      .fold(
        _ => buildAnswer(localIp, localRtpPort),
        _.description.render
      )
  }

  /** Build a minimal SDP offer. */
  def buildOffer(
      localIp: IpAddress,
      localRtpPort: Port,
      direction: MediaDirection = MediaDirection.SendRecv
  ): String =
    OfferAnswer
      .offer(
        LocalMedia(localIp, localRtpPort, 0, 0),
        Capabilities.Default,
        direction
      )
      .render

  /** Build a minimal SDP answer, offering the same fixed codec set as
    * `buildOffer`.
    */
  def buildAnswer(localIp: IpAddress, localRtpPort: Port): String =
    buildOffer(localIp, localRtpPort)
}
