package softphone4s.model

import cats.syntax.all.*
import cats.{Eq, Show}
import com.comcast.ip4s.{Host, IpAddress, Port}

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
