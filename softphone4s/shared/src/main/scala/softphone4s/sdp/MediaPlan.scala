package softphone4s.sdp

import com.comcast.ip4s.{IpAddress, SocketAddress}
import softphone4s.model.{MediaDirection, PayloadType}
import softphone4s.srtp.SrtpPlan

/** What was agreed for the audio of a session: where to send, in which codec,
  * and which way media flows.
  *
  * @param remoteDirection
  *   as the other side stated it; `sendonly` and `inactive` mean it will not
  *   receive, which is a hold.
  * @param direction
  *   ours.
  * @param comfortNoiseType
  *   the payload type the other side may send comfort noise on, if we agreed to
  *   receive it.
  * @param srtp
  *   the keys, if the media is encrypted.
  */
final case class MediaPlan(
    remote: SocketAddress[IpAddress],
    codec: Codec,
    payloadType: PayloadType,
    telephoneEventType: Option[PayloadType],
    remoteDirection: MediaDirection,
    direction: MediaDirection,
    comfortNoiseType: Option[PayloadType] = None,
    srtp: Option[SrtpPlan] = None
) {

  /** Whether the other side has put the call on hold: it will not receive. */
  def isHold: Boolean = !remoteDirection.receives
}
