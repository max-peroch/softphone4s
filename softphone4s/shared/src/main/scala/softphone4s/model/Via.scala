package softphone4s.model

import cats.syntax.all.*
import cats.{Eq, Show}
import com.comcast.ip4s.{Host, IpAddress, Port, SocketAddress, port}

import scala.collection.immutable.ListMap

/** One entry of a Via header (RFC 3261 §20.42): `SIP/2.0/UDP host:port;...`.
  * `branch`, `rport` and `received` are typed; `params` holds the rest.
  */
final case class Via(
    transport: TransportProtocol,
    host: Host,
    port: Option[Port],
    branch: Option[Branch],
    rport: Option[RPort],
    received: Option[IpAddress],
    params: HeaderParams
) {

  def render: String = {
    val branchText = branch.fold("")(b => s";branch=${b.value}")
    val rportText  = rport.fold("") {
      case RPort.Requested   => ";rport"
      case RPort.Assigned(p) => s";rport=${p.value}"
    }
    val receivedText = received.fold("")(ip => s";received=$ip")
    s"SIP/2.0/${transport.token.toUpperCase} ${HostPort.render(host, port)}" +
      branchText + rportText + receivedText + HeaderSyntax.renderParams(params)
  }

  override def toString: String = render

  /** This Via asking the peer to send requests for its sent-by address back on
    * the connection this one came in on (RFC 5923).
    */
  def withAlias: Via = copy(params = params.updated(ParamName.Alias, None))

  def hasAlias: Boolean = params.contains(ParamName.Alias)

  /** This Via asking the server to answer to the address and port the request
    * came from, and to say what they were (RFC 3581).
    */
  def requestingRport: Via = copy(rport = Some(RPort.Requested))

  /** This Via as the server that received the request from `source` records it
    * in its response: with `rport` filled in if it was asked for (RFC 3581),
    * and `received` set if `rport` was asked for or the sent-by is not
    * `source`'s address (RFC 3261 §18.2.1).
    */
  def seenFrom(source: SocketAddress[IpAddress]): Via = {
    val asked  = rport.contains(RPort.Requested)
    val filled =
      if asked then copy(rport = Some(RPort.Assigned(source.port))) else this
    val differs = sentBy.forall(_.host != source.host)
    if asked || differs then filled.copy(received = Some(source.host))
    else filled
  }

  /** The address the server that answered saw this request come from, as its
    * `received` and `rport` say (RFC 3581): what we look like from outside. Any
    * part it does not give is ours from the sent-by. None when it says nothing
    * of the kind.
    */
  def observedAddress: Option[SocketAddress[IpAddress]] = {
    val assigned = rport.collect { case RPort.Assigned(p) => p }
    if received.isEmpty && assigned.isEmpty then None
    else
      for {
        ip   <- received.orElse(sentBy.map(_.host))
        port <- assigned.orElse(sentBy.map(_.port))
      } yield SocketAddress(ip, port)
  }

  /** Where the sender says it can be reached, if that is an IP address: its
    * sent-by, with the transport's default port when none is given.
    */
  def sentBy: Option[SocketAddress[IpAddress]] =
    host match {
      case ip: IpAddress =>
        Some(
          SocketAddress(
            ip,
            port.getOrElse(
              transport match {
                case TransportProtocol.Tls | TransportProtocol.Wss => port"5061"
                case _                                             => port"5060"
              }
            )
          )
        )
      case _ => None
    }
}

object Via {

  private val ViaPrefix = """(?i)SIP\s*/\s*2\.0\s*/\s*([A-Za-z]+)\s+(.+)""".r

  /** A Via for a request we send: transport, our address and the branch. */
  def request(
      transport: TransportProtocol,
      host: Host,
      port: Port,
      branch: Branch
  ): Via =
    Via(
      transport,
      host,
      Some(port),
      Some(branch),
      None,
      None,
      ListMap.empty
    )

  def parse(value: String): Either[String, Via] = {
    val (head, paramText) = HeaderSyntax.firstUnquoted(value, ';') match {
      case None      => (value, "")
      case Some(idx) => (value.take(idx), value.drop(idx + 1))
    }
    for {
      parts <- head.trim match {
        case ViaPrefix(proto, sentBy) => Right((proto, sentBy))
        case _                        => Left(s"malformed Via: '$value'")
      }
      transport <- TransportProtocol
        .fromToken(parts._1)
        .toRight(s"unsupported Via transport: '${parts._1}'")
      hostPort <- HostPort.parse(parts._2).left.map(e => s"$e in Via: '$value'")
      all      <- HeaderSyntax.parseParams(paramText)
      branch <- all.get(ParamName.Branch).flatten.traverse(v => Branch(v.value))
      rport  <- parseRPort(all.get(ParamName.RPort))
      received <- all
        .get(ParamName.Received)
        .flatten
        .traverse(v =>
          IpAddress
            .fromString(v.value)
            .toRight(s"invalid received address: '${v.value}'")
        )
    } yield Via(
      transport,
      hostPort._1,
      hostPort._2,
      branch,
      rport,
      received,
      all
        .removed(ParamName.Branch)
        .removed(ParamName.RPort)
        .removed(ParamName.Received)
    )
  }

  private def parseRPort(
      value: Option[Option[HeaderParamValue]]
  ): Either[String, Option[RPort]] =
    value match {
      case None          => Right(None)
      case Some(None)    => Right(Some(RPort.Requested))
      case Some(Some(v)) =>
        Port
          .fromString(v.value)
          .map(p => Some(RPort.Assigned(p)))
          .toRight(s"invalid rport: '${v.value}'")
    }

  given Eq[Via]   = Eq.fromUniversalEquals
  given Show[Via] = Show.show(_.render)
}
