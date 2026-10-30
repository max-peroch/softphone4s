package softphone4s.model

import cats.{Eq, Show}
import cats.syntax.all.*
import com.comcast.ip4s.{Host, Port}

import scala.collection.immutable.ListMap

/** A `sip:` or `sips:` URI (RFC 3261 §19.1).
  *
  * Every component is a validated type, and a password cannot exist without a
  * user, so the only way to build a `SipUri` is through [[SipUri.from]] or
  * [[SipUri.parse]].
  *
  * `transport` and `routing` (the `lr` flag) are typed fields, not entries of
  * `params`, which holds the remaining parameters in order of appearance. They
  * render in that order: transport, `lr`, other parameters. Parameter names are
  * case-insensitive and stored lower-cased; user, password and values stay
  * percent-encoded as written.
  */
final case class SipUri private (
    scheme: SipScheme,
    userInfo: Option[UserInfo],
    host: Host,
    port: Option[Port],
    transport: Option[Transport],
    routing: Routing,
    params: ListMap[ParamName, Option[ParamValue]],
    headers: List[UriHeader]
) {

  def user: Option[User] = userInfo.map(_.user)

  /** The value of a parameter other than `transport` and `lr`: `Some(None)` for
    * a flag, `None` when absent.
    */
  def param(name: ParamName): Option[Option[ParamValue]] = params.get(name)

  def withTransport(t: Transport): SipUri = copy(transport = Some(t))

  def withLooseRouting: SipUri = copy(routing = Routing.Loose)

  def isLooseRouting: Boolean = routing == Routing.Loose

  /** Sets or replaces a parameter; `transport` and `lr` have their own fields.
    */
  def withParam(
      name: ParamName,
      value: Option[ParamValue] = None
  ): Either[String, SipUri] =
    Either.cond(
      !SipUri.Reserved.contains(name),
      copy(params = params.updated(name, value)),
      s"'${name.value}' is a typed field of SipUri, not a free parameter"
    )

  /** The URI without parameters and headers. */
  def bare: SipUri =
    copy(
      transport = None,
      routing = Routing.Strict,
      params = ListMap.empty,
      headers = Nil
    )

  def render: String = {
    val userText = userInfo.fold("") { ui =>
      ui.user.value + ui.password.fold("")(":" + _.value) + "@"
    }
    val hostText = host.toString match {
      case h if h.contains(':') => s"[$h]"
      case h                    => h
    }
    val portText   = port.fold("")(p => s":${p.value}")
    val paramTexts =
      transport.map(t => s";transport=${t.token}").toList ++
        Option.when(isLooseRouting)(";lr") ++
        params.map {
          case (n, Some(v)) => s";${n.value}=${v.value}"
          case (n, None)    => s";${n.value}"
        }
    val headerText =
      if headers.isEmpty then ""
      else
        headers
          .map(h => s"${h.name.value}=${h.value.value}")
          .mkString("?", "&", "")
    s"${scheme.value}:$userText$hostText$portText${paramTexts.mkString}$headerText"
  }

  override def toString: String = render
}

object SipUri {

  private val Reserved = Set(ParamName.Transport, ParamName.Lr)

  /** Builds a URI from validated parts; fails if `params` contains `transport`
    * or `lr`, which belong in their own fields.
    */
  def from(
      scheme: SipScheme,
      host: Host,
      userInfo: Option[UserInfo] = None,
      port: Option[Port] = None,
      transport: Option[Transport] = None,
      routing: Routing = Routing.Strict,
      params: ListMap[ParamName, Option[ParamValue]] = ListMap.empty,
      headers: List[UriHeader] = Nil
  ): Either[String, SipUri] =
    Either.cond(
      !params.keysIterator.exists(Reserved.contains),
      new SipUri(
        scheme,
        userInfo,
        lowerCased(host),
        port,
        transport,
        routing,
        params,
        headers
      ),
      "'transport' and 'lr' are typed fields of SipUri, not free parameters"
    )

  /** Host names are case-insensitive (§19.1.4) but `Host` equality is not. */
  private def lowerCased(host: Host): Host =
    Host.fromString(host.toString.toLowerCase).getOrElse(host)

  /** Per RFC 3261 §19.1.4, parameter and header order does not matter, which
    * `ListMap` equality already ignores; the header list is compared as is.
    */
  given Eq[SipUri]   = Eq.fromUniversalEquals
  given Show[SipUri] = Show.show(_.render)

  /** The URI of a `From`, `To` or `Contact` header value: the text inside
    * `<...>` (after an optional display name), or for a bare addr-spec the text
    * before the first `;`, since parameters there belong to the header.
    */
  def fromHeaderValue(value: String): Either[String, SipUri] = {
    val v = value.trim
    parse(
      if v.contains('<') then v.dropWhile(_ != '<').drop(1).takeWhile(_ != '>')
      else v.takeWhile(_ != ';')
    )
  }

  def parse(input: String): Either[String, SipUri] = {
    val s = input.trim
    s.indexOf(':') match {
      case -1  => Left(s"missing scheme: $input")
      case idx =>
        schemeOf(s.take(idx)).toRight(s"unsupported scheme: $input").flatMap {
          parseBody(_, s.drop(idx + 1), input)
        }
    }
  }

  private def schemeOf(text: String): Option[SipScheme] =
    text.toLowerCase match {
      case "sip"  => Some(SipScheme.Sip)
      case "sips" => Some(SipScheme.Sips)
      case _      => None
    }

  private def parseBody(
      scheme: SipScheme,
      body: String,
      input: String
  ): Either[String, SipUri] = {
    val (beforeHeaders, headerText) = splitOnce(body, '?')
    // The user part never contains a raw '@', but URI headers can.
    val (userText, afterUser) = beforeHeaders.indexOf('@') match {
      case -1  => (None, beforeHeaders)
      case idx => (Some(beforeHeaders.take(idx)), beforeHeaders.drop(idx + 1))
    }
    val (hostPort, paramText) = splitOnce(afterUser, ';')
    for {
      userInfo <- userText.traverse(parseUserInfo)
      hp       <- parseHostPort(hostPort, input)
      params   <- paramText.fold(Right(Nil))(parseParams)
      typed    <- extractTyped(params)
      hdrs     <- headerText.fold(Right(Nil))(parseHeaders)
      uri      <- from(
        scheme,
        hp._1,
        userInfo,
        hp._2,
        typed._1,
        typed._2,
        typed._3,
        hdrs
      )
    } yield uri
  }

  private def parseUserInfo(text: String): Either[String, UserInfo] = {
    val (user, password) = splitOnce(text, ':')
    (User(user), password.traverse(Password(_))).mapN(UserInfo(_, _))
  }

  private def parseHostPort(
      text: String,
      input: String
  ): Either[String, (Host, Option[Port])] = {
    val (hostText, portText) =
      if text.startsWith("[") then
        text.indexOf(']') match {
          case -1  => (text, None)
          case end =>
            (text.slice(1, end), Some(text.drop(end + 1)).filter(_.nonEmpty))
        }
      else
        text.lastIndexOf(':') match {
          case -1  => (text, None)
          case idx => (text.take(idx), Some(text.drop(idx)))
        }
    for {
      host <- Host.fromString(hostText).toRight(s"invalid host: $input")
      port <- portText.traverse(p =>
        p.stripPrefix(":")
          .toIntOption
          .flatMap(Port.fromInt)
          .toRight(s"invalid port: $input")
      )
    } yield (host, port)
  }

  private def parseParams(
      text: String
  ): Either[String, List[(ParamName, Option[ParamValue])]] =
    text.split(';').toList.traverse { segment =>
      val (name, value) = splitOnce(segment, '=')
      (ParamName(name), value.traverse(ParamValue(_))).tupled
    }

  /** Splits the parsed parameters into the typed `transport` and `lr` fields
    * and the remaining ones; a repeated parameter is an error (§19.1.1).
    */
  private def extractTyped(
      all: List[(ParamName, Option[ParamValue])]
  ): Either[
    String,
    (Option[Transport], Routing, ListMap[ParamName, Option[ParamValue]])
  ] = {
    val names = all.map(_._1)
    for {
      _ <- Either.cond(
        names.distinct.size == names.size,
        (),
        "a URI parameter appears more than once"
      )
      byName = ListMap.from(all)
      transport <- byName.get(ParamName.Transport) match {
        case None          => Right(None)
        case Some(None)    => Left("transport needs a value")
        case Some(Some(v)) =>
          Transport
            .fromToken(v.value)
            .map(Some(_))
            .toRight(s"unsupported transport: ${v.value}")
      }
      routing <- byName.get(ParamName.Lr) match {
        case None          => Right(Routing.Strict)
        case Some(None)    => Right(Routing.Loose)
        case Some(Some(_)) => Left("lr is a flag and takes no value")
      }
    } yield (
      transport,
      routing,
      byName.removed(ParamName.Transport).removed(ParamName.Lr)
    )
  }

  private def parseHeaders(text: String): Either[String, List[UriHeader]] =
    text.split('&').toList.traverse { segment =>
      splitOnce(segment, '=') match {
        case (name, Some(value)) =>
          (HeaderName(name), HeaderValue(value)).mapN(UriHeader(_, _))
        case (_, None) => Left(s"URI header without a value: '$segment'")
      }
    }

  private def splitOnce(s: String, sep: Char): (String, Option[String]) =
    s.indexOf(sep) match {
      case -1  => (s, None)
      case idx => (s.take(idx), Some(s.drop(idx + 1)))
    }
}
