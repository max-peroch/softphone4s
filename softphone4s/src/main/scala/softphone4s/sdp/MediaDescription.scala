package softphone4s.sdp

import com.comcast.ip4s.{IpAddress, Port}
import softphone4s.model.{MediaDirection, PayloadType}

/** One `m=` section. A `port` of 0 is a stream that is declined (RFC 3264 §6).
  */
final case class MediaDescription(
    kind: MediaKind,
    port: Port,
    protocol: MediaProtocol,
    formats: List[String],
    connection: Option[IpAddress],
    attributes: List[Attribute]
) {

  def isDeclined: Boolean = port.value == 0

  /** The formats that are RTP payload types. */
  def payloadTypes: List[PayloadType] =
    formats.flatMap(_.toIntOption).flatMap(PayloadType.fromInt)

  def rtpMaps: List[RtpMap] =
    attributes.collect { case Attribute("rtpmap", Some(value)) =>
      RtpMap.parse(value)
    }.flatten

  /** The format parameters of a payload type, from `a=fmtp`. */
  def fmtp(payloadType: PayloadType): Option[String] =
    attributes.collectFirst {
      case Attribute("fmtp", Some(value))
          if value.trim.startsWith(s"$payloadType ") =>
        value.trim.drop(payloadType.toString.length).trim
    }

  /** What each offered payload type means, in the order they are listed: its
    * `rtpmap`, else the assignment of RFC 3551.
    */
  def meanings: List[RtpMap] = {
    val mapped = rtpMaps.map(m => m.payloadType -> m).toMap
    payloadTypes.flatMap(pt => mapped.get(pt).orElse(RtpMap.Static.get(pt)))
  }

  /** Whether telephone events (RFC 4733) are offered on `payloadType`. */
  def offersTelephoneEvents(payloadType: PayloadType): Boolean =
    payloadTypes.contains(payloadType) &&
      rtpMaps.exists(m =>
        m.payloadType == payloadType &&
          m.encoding.equalsIgnoreCase("telephone-event") && m.clockRate == 8000
      )

  /** The direction this section states, if it does. */
  def direction: Option[MediaDirection] =
    MediaDescription.directionIn(attributes)

  def render(addressType: IpAddress => String): List[String] =
    s"m=${kind.token} ${port.value} ${protocol.token} ${formats.mkString(" ")}" ::
      connection.toList.map(c => s"c=IN ${addressType(c)} $c") :::
      attributes.map(_.render)
}

object MediaDescription {

  /** The direction among `attributes`, if one is stated. */
  def directionIn(attributes: List[Attribute]): Option[MediaDirection] =
    attributes.collectFirst(Function.unlift {
      case Attribute(name, None) => MediaDirection.fromAttribute(name)
      case _                     => None
    })
}
