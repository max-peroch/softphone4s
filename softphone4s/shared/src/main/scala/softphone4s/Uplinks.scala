package softphone4s

import cats.data.NonEmptyList
import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import org.typelevel.log4cats.StructuredLogger
import softphone4s.logging.LogFields
import softphone4s.dns.{ResolvedTarget, TargetResolver}
import softphone4s.model.*

/** The ways to reach a server: where its name leads (RFC 3263), the other ways
  * to try when one gets no answer, the way over TCP for a request too big for a
  * datagram, and the next hop of a request within a dialog.
  */
private[softphone4s] final class Uplinks[F[_]: Async](
    targets: TargetResolver[F],
    endpoints: EndpointFactory[F],
    logger: StructuredLogger[F]
) {

  /** Where a request within a dialog that runs over `transport` goes when its
    * next hop is `uri`: the Contact or the first Route, located as RFC 3263
    * says (an address or a port in it is used as it is, otherwise SRV and NAPTR
    * records decide). The request carries a Via for `transport`, so only a
    * target reached that way will do; for any other the caller falls back to
    * the server it dialled.
    */
  private[softphone4s] def nextHopOver(
      transport: TransportProtocol
  )(uri: SipUri): F[SocketAddress[IpAddress]] =
    targets
      .resolve(uri, transport)
      .flatMap(found =>
        Async[F].fromOption(
          found.find(_.transport == transport).map(_.address),
          SoftphoneError.HostResolutionFailed(uri.host.toString)
        )
      )

  /** Every place to try for `uri`, best first, at most
    * `SoftphoneStreaming.MaxTargets`.
    */
  private[softphone4s] def allTargets(
      account: Account,
      uri: SipUri
  ): F[NonEmptyList[ResolvedTarget]] =
    targets
      .resolve(aimed(account, uri), account.config.transport)
      .flatMap(found =>
        Async[F].fromOption(
          NonEmptyList.fromList(found.take(SoftphoneStreaming.MaxTargets)),
          SoftphoneError.HostResolutionFailed(uri.host.toString)
        )
      )

  /** The way over TCP to the server that `endpoint` reaches over UDP: where a
    * request too big for a datagram goes (RFC 3261 §18.1.1). The server is
    * looked up for TCP, as RFC 3263 says, so a domain that serves TCP on
    * another port, or elsewhere, is found; with no answer it is the UDP address
    * again. None for a call that is not over UDP.
    */
  private[softphone4s] def streamWayTo(
      account: Account,
      target: SipUri,
      endpoint: LocalEndpoint,
      peer: SocketAddress[IpAddress]
  ): F[Option[Uplink]] =
    if endpoint.transport != TransportProtocol.Udp then Async[F].pure(None)
    else
      (for {
        found <- targets.resolve(
          aimed(account, target).withTransport(TransportProtocol.Tcp),
          TransportProtocol.Tcp
        )
        address = found
          .find(_.transport == TransportProtocol.Tcp)
          .fold(peer)(_.address)
        ip     <- endpoints.localIpFor(address)
        stream <- Async[F].fromEither(endpoint.via(ip, TransportProtocol.Tcp))
      } yield Option(Uplink(stream, address))).handleError(_ => None)

  /** The other ways to reach the server, each as `endpoint` over that target's
    * transport and from the local address that routes to it, keeping the RTP
    * port. A target we cannot speak to, or cannot build an endpoint for, is
    * left out.
    */
  private[softphone4s] def alternativesTo(
      endpoint: LocalEndpoint,
      others: List[ResolvedTarget]
  ): F[List[Uplink]] =
    others.flatTraverse { other =>
      (for {
        transport <- endpoints.available(other.transport)
        ip        <- endpoints.localIpFor(other.address)
        rebased   <- Async[F].fromEither(endpoint.via(ip, transport))
      } yield List(Uplink(rebased, other.address))).handleErrorWith(error =>
        logger
          .debug(
            Map(
              LogFields.Target -> other.address.toString,
              LogFields.Error  -> String.valueOf(error.getMessage)
            )
          )("Not using this target as a fallback")
          .as(Nil)
      )
    }

  /** `uri`, at the account's server port if that is set to something other than
    * the default 5060 and the URI names no port: a port given is taken as it is
    * and skips the SRV lookup, while the default leaves DNS to say.
    */
  private def aimed(account: Account, uri: SipUri): SipUri =
    if uri.port.isEmpty && account.config.serverPort != port"5060" then
      uri.withPort(account.config.serverPort)
    else uri

  /** The ways to reach the server for a request to `target`, best first: the
    * first must work, and the others, which are there for when it does not, are
    * left out if they cannot be set up.
    */
  private[softphone4s] def waysTo(
      account: Account,
      target: SipUri
  ): F[NonEmptyList[Uplink]] =
    for {
      found  <- allTargets(account, target)
      first  <- uplinkTo(account, target, found.head)
      others <- found.tail.flatTraverse(other =>
        uplinkTo(account, target, other)
          .map(List(_))
          .handleErrorWith(error =>
            logger
              .debug(
                Map(
                  LogFields.Target -> other.address.toString,
                  LogFields.Error  -> String.valueOf(error.getMessage)
                )
              )("Not using this target as a fallback")
              .as(Nil)
          )
      )
    } yield NonEmptyList(first, others)

  /** The way to reach `resolved`, as `account`, for a request to `target`. */
  private[softphone4s] def uplinkTo(
      account: Account,
      target: SipUri,
      resolved: ResolvedTarget
  ): F[Uplink] =
    for {
      protocol <- endpoints.available(resolved.transport)
      localIp  <- endpoints.localIpFor(resolved.address)
      endpoint <- endpoints
        .signallingEndpoint(account, localIp, protocol)
        .flatMap(endpoints.secureIfNeeded(_, target.scheme))
        .flatMap(endpoints.seenFromOutside)
    } yield Uplink(endpoint, resolved.address)

}
