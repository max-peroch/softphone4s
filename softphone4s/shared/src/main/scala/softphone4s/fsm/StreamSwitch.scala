package softphone4s.fsm

import cats.data.NonEmptyList
import softphone4s.codec.SipCodec
import softphone4s.model.*

/** Moving a request that does not fit in a UDP datagram to a stream transport
  * (RFC 3261 §18.1.1). The only parts of a request that name the transport are
  * its top Via and its Contact, and both are ours, so a request already built
  * for UDP can be made ready for TCP without being built again.
  */
object StreamSwitch {

  /** Whether `request` is bound for UDP and longer than `limit` bytes. */
  def tooBigForUdp(request: SipRequest, limit: Int): Boolean =
    request.headers.topVia.transport == TransportProtocol.Udp &&
      SipCodec.encode(request).size > limit

  /** `request` as it travels over `stream`'s transport: our Via and Contact are
    * those of `stream`, and everything else, the branch included, stays. It is
    * the same transaction, only carried differently.
    */
  def over(request: SipRequest, stream: LocalEndpoint): SipRequest = {
    val headers = request.headers
    val via     = headers.topVia.copy(
      transport = stream.transport,
      host = stream.ip,
      port = Some(stream.sipPort)
    )
    request.copy(headers =
      headers.copy(
        vias = NonEmptyList(
          if stream.transport == TransportProtocol.Udp then via
          else via.withAlias,
          headers.vias.tail
        ),
        contacts = headers.contacts.map(_ => NameAddr(stream.contact))
      )
    )
  }

  /** `request` as a new transaction: the same, with `branch` in its top Via.
    * For sending it again when the first try could not be made.
    */
  def reissued(request: SipRequest, branch: Branch): SipRequest =
    request.copy(headers =
      request.headers.copy(vias =
        NonEmptyList(
          request.headers.topVia.copy(branch = Some(branch)),
          request.headers.vias.tail
        )
      )
    )
}
