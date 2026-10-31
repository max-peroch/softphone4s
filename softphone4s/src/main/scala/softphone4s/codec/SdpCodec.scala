package softphone4s.codec

import com.comcast.ip4s.{IpAddress, Port, ip, port}
import softphone4s.model.{MediaDirection, PayloadType}
import softphone4s.srtp.SrtpPolicy
import softphone4s.sdp.{
  Capabilities,
  LocalMedia,
  MediaKind,
  MediaPlan,
  MediaProtocol,
  OfferAnswer,
  Reject,
  SessionDescription
}

/** The SDP the call code deals in: one audio stream, in the codecs we have. It
  * reads and writes through [[softphone4s.sdp]], which does the parsing and the
  * offer/answer; this is the shape the dialogs were written against.
  */
object SdpCodec {

  /** A local address that stands for ours when only the outcome of an answer
    * matters.
    */
  private val probe = LocalMedia(ip"0.0.0.0", port"0", 0, 0)

  /** What a caller's SDP offers for audio, and that we can answer. */
  final case class AudioOffer(
      ip: IpAddress,
      port: Port,
      payloadTypes: List[PayloadType],
      offersTelephoneEvent: Boolean,
      direction: MediaDirection,
      description: SessionDescription
  ) {

    /** Whether this offer puts the call on hold: nothing is to be sent to its
      * sender (`sendonly`, `inactive`, or the old connection address 0.0.0.0).
      */
    def isHold: Boolean = !direction.receives
  }

  /** The audio an SDP body offers, if it has a stream we can address and answer
    * with `capabilities`: one that is not declined, is plain RTP, has an
    * address and a codec of ours.
    */
  def parseOffer(
      sdp: String,
      capabilities: Capabilities
  ): Option[AudioOffer] =
    for {
      description <- SessionDescription.parse(sdp).toOption
      section     <- description.media.find(m =>
        m.kind == MediaKind.Audio && !m.isDeclined &&
          (m.protocol == MediaProtocol.RtpAvp ||
            (m.protocol == MediaProtocol.RtpSavp &&
              capabilities.crypto.policy != SrtpPolicy.Off))
      )
      ip <- description.addressOf(section)
      _  <- OfferAnswer.answer(description, probe, capabilities).toOption
    } yield AudioOffer(
      ip,
      section.port,
      section.payloadTypes,
      section.offersTelephoneEvents(PayloadType.TelephoneEvent),
      description.directionOf(section),
      description
    )

  /** What answering `offer` settles. */
  def planFor(
      offer: AudioOffer,
      capabilities: Capabilities
  ): Either[Reject, MediaPlan] =
    OfferAnswer.answer(offer.description, probe, capabilities).map(_.plan)

  /** What an answer to the offer we made settles. A body that is not SDP has no
    * audio in it.
    */
  def planFromAnswer(
      sdp: String,
      capabilities: Capabilities,
      direction: MediaDirection
  ): Either[Reject, MediaPlan] =
    SessionDescription
      .parse(sdp)
      .left
      .map(_ => Reject.NoAudio)
      .flatMap(OfferAnswer.accept(_, capabilities, direction))

  /** An answer to `offer`: our first codec among those offered, and
    * telephone-event only if it was offered, so the answer stays within what
    * the caller proposed (RFC 3264 §6).
    */
  def buildAnswerTo(
      offer: AudioOffer,
      localIp: IpAddress,
      localRtpPort: Port,
      capabilities: Capabilities
  ): String =
    answerText(offer.description, localIp, localRtpPort, capabilities)

  /** The answer to the offer in `sdp`, which was in a 200: it goes in our ACK
    * (RFC 3261 §14.2). If it cannot be answered, an offer of our own stands in.
    */
  def buildAnswerToBody(
      sdp: String,
      localIp: IpAddress,
      localRtpPort: Port,
      capabilities: Capabilities
  ): String =
    SessionDescription
      .parse(sdp)
      .fold(
        _ => buildOffer(localIp, localRtpPort, capabilities),
        answerText(_, localIp, localRtpPort, capabilities)
      )

  private def answerText(
      offered: SessionDescription,
      localIp: IpAddress,
      localRtpPort: Port,
      capabilities: Capabilities
  ): String =
    OfferAnswer
      .answer(
        offered,
        LocalMedia(localIp, localRtpPort, 0, 0),
        capabilities
      )
      .fold(
        _ => buildOffer(localIp, localRtpPort, capabilities),
        _.description.render
      )

  /** An SDP offer of everything in `capabilities`. */
  def buildOffer(
      localIp: IpAddress,
      localRtpPort: Port,
      capabilities: Capabilities,
      direction: MediaDirection = MediaDirection.SendRecv
  ): String =
    OfferAnswer
      .offer(
        LocalMedia(localIp, localRtpPort, 0, 0),
        capabilities,
        direction
      )
      .render
}
