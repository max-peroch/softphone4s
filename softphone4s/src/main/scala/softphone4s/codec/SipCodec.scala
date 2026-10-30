package softphone4s.codec

import cats.data.NonEmptyList
import cats.syntax.all.*
import softphone4s.model.*

/** Pure SIP text codec — no side effects. */
object SipCodec {

  private val CRLF = "\r\n"

  // -------------------------------------------------------------------------
  // Encode
  // -------------------------------------------------------------------------

  def encode(msg: SipMessage): String = msg match {
    case r: SipRequest  => encodeRequest(r)
    case r: SipResponse => encodeResponse(r)
  }

  private def encodeRequest(r: SipRequest): String = {
    val requestLine =
      s"${r.method.toString.toUpperCase} ${r.requestUri.render} SIP/2.0"
    s"$requestLine$CRLF${encodeHeaders(r.headers, r.body)}$CRLF${r.body}"
  }

  private def encodeResponse(r: SipResponse): String = {
    val statusLine = s"SIP/2.0 ${r.statusCode} ${r.reason}"
    s"$statusLine$CRLF${encodeHeaders(r.headers, r.body)}$CRLF${r.body}"
  }

  private val DefaultMaxForwards = 70

  private def encodeHeaders(headers: SipHeaders, body: String): String = {
    val out = new StringBuilder
    headers.vias.toList.foreach(via => out.append(s"Via: ${via.render}$CRLF"))
    out.append(s"From: ${headers.from.render}$CRLF")
    out.append(s"To: ${headers.to.render}$CRLF")
    out.append(s"Call-ID: ${headers.callId.value}$CRLF")
    out.append(
      s"CSeq: ${headers.cseq.seq} ${headers.cseq.method.toString.toUpperCase}$CRLF"
    )
    headers.contacts.foreach(c => out.append(s"Contact: ${c.render}$CRLF"))
    out.append(s"Max-Forwards: ${headers.maxForwards}$CRLF")
    headers.routes.foreach(r => out.append(s"Route: ${r.render}$CRLF"))
    headers.recordRoutes.foreach(r =>
      out.append(s"Record-Route: ${r.render}$CRLF")
    )
    headers.wwwAuthenticate.foreach(v =>
      out.append(s"WWW-Authenticate: $v$CRLF")
    )
    headers.proxyAuthenticate.foreach(v =>
      out.append(s"Proxy-Authenticate: $v$CRLF")
    )
    headers.contentType.foreach(contentType =>
      out.append(s"Content-Type: $contentType$CRLF")
    )
    val bodyLength = body.getBytes("UTF-8").length
    out.append(s"Content-Length: $bodyLength$CRLF")
    headers.others.foreach(h => out.append(s"${h.name.value}: ${h.value}$CRLF"))
    out.toString
  }

  // -------------------------------------------------------------------------
  // Decode
  // -------------------------------------------------------------------------

  def decode(raw: String): Either[String, SipMessage] = {
    val lines = raw.split("\r\n", -1).toList
    lines match {
      case Nil          => Left("empty message")
      case head :: rest =>
        if head.startsWith("SIP/2.0") then parseResponse(head, rest)
        else parseRequest(head, rest)
    }
  }

  private def parseRequest(
      requestLine: String,
      rest: List[String]
  ): Either[String, SipMessage] =
    parseRequestLine(requestLine).flatMap { (method, uri) =>
      val (headerLines, bodyLines) = splitHeadersBody(rest)
      parseHeaders(headerLines)
        .map(headers =>
          SipRequest(method, uri, headers, bodyLines.mkString(CRLF))
        )
    }

  private def parseRequestLine(
      line: String
  ): Either[String, (SipMethod, SipUri)] =
    line.split(" ", 3) match {
      case Array(methodStr, uri, "SIP/2.0") =>
        (parseMethod(methodStr), SipUri.parse(uri)).tupled
      case _ => Left(s"bad request line: $line")
    }

  private def parseResponse(
      statusLine: String,
      rest: List[String]
  ): Either[String, SipMessage] =
    parseStatusLine(statusLine).flatMap { (code, reason) =>
      val (headerLines, bodyLines) = splitHeadersBody(rest)
      parseHeaders(headerLines)
        .map(headers =>
          SipResponse(code, reason, headers, bodyLines.mkString(CRLF))
        )
    }

  private def parseStatusLine(line: String): Either[String, (Int, String)] =
    line.split(" ", 3) match {
      case Array("SIP/2.0", codeStr, reason) =>
        codeStr.toIntOption
          .toRight(s"bad status code: $codeStr")
          .map(_ -> reason)
      case _ => Left(s"bad status line: $line")
    }

  private def splitHeadersBody(
      lines: List[String]
  ): (List[String], List[String]) = {
    val emptyLineIndex = lines.indexWhere(_.isEmpty)
    if emptyLineIndex < 0 then (lines, Nil)
    else (lines.take(emptyLineIndex), lines.drop(emptyLineIndex + 1))
  }

  // RFC 3261 §7.3.3 compact forms, plus the long names we model.
  private val CompactForms = Map(
    "v" -> "via",
    "f" -> "from",
    "t" -> "to",
    "i" -> "call-id",
    "m" -> "contact",
    "c" -> "content-type",
    "l" -> "content-length"
  )

  private val Modelled = Set(
    "call-id",
    "from",
    "to",
    "via",
    "cseq",
    "contact",
    "max-forwards",
    "content-type",
    "content-length",
    "route",
    "record-route",
    "www-authenticate",
    "proxy-authenticate"
  )

  private def canonical(name: String): String = {
    val lower = name.toLowerCase
    CompactForms.getOrElse(lower, lower)
  }

  private def parseHeaders(lines: List[String]): Either[String, SipHeaders] = {
    // Unfold multi-line headers (continuation lines start with whitespace).
    // foldLeft so continuations are appended to the preceding line, not the following one.
    val unfolded = lines
      .foldLeft(List.empty[String]) {
        case (prev :: rest, line) if line.headOption.exists(_.isWhitespace) =>
          (prev + " " + line.trim) :: rest
        case (acc, line) => line :: acc
      }
      .reverse

    val fields = unfolded.flatMap(parseHeaderLine)
    val byName = fields.groupMap((name, _) => canonical(name))(_._2)

    def values(name: String): List[String] = byName.getOrElse(name, Nil)
    // Via, Contact, Route and Record-Route may carry several comma-separated
    // values in one line.
    def listValues(name: String): List[String] =
      values(name)
        .flatMap(HeaderSyntax.split(_, ','))
        .map(_.trim)
        .filter(_.nonEmpty)
    def required(name: String, label: String): Either[String, String] =
      values(name).headOption.map(_.trim).toRight(s"missing $label header")

    val callId =
      required("call-id", "Call-ID").flatMap(CallId(_)).toValidatedNel
    val from = required("from", "From").flatMap(NameAddr.parse).toValidatedNel
    val to   = required("to", "To").flatMap(NameAddr.parse).toValidatedNel
    val vias = NonEmptyList
      .fromList(listValues("via"))
      .toRight("missing Via header")
      .flatMap(_.traverse(Via.parse))
      .toValidatedNel
    val cseq = required("cseq", "CSeq")
      .flatMap(v => parseCseq(v).toRight(s"bad CSeq: '$v'"))
      .toValidatedNel
    val contacts = listValues("contact").traverse(NameAddr.parse).toValidatedNel
    val routes   = listValues("route").traverse(NameAddr.parse).toValidatedNel
    val recordRoutes =
      listValues("record-route").traverse(NameAddr.parse).toValidatedNel
    val others = fields
      .filterNot((name, _) => Modelled.contains(canonical(name)))
      .traverse((name, value) => FieldName(name).map(ExtensionHeader(_, value)))
      .toValidatedNel

    (callId, from, to, vias, cseq, contacts, routes, recordRoutes, others)
      .mapN {
        (
            callId,
            from,
            to,
            vias,
            cseq,
            contacts,
            routes,
            recordRoutes,
            others
        ) =>
          SipHeaders(
            callId = callId,
            from = from,
            to = to,
            vias = vias,
            cseq = cseq,
            contacts = contacts,
            maxForwards = values("max-forwards").headOption
              .flatMap(_.trim.toIntOption)
              .getOrElse(DefaultMaxForwards),
            contentType = values("content-type").headOption,
            routes = routes,
            recordRoutes = recordRoutes,
            wwwAuthenticate = values("www-authenticate").headOption,
            proxyAuthenticate = values("proxy-authenticate").headOption,
            others = others
          )
      }
      .toEither
      .left
      .map(_.mkString_("; "))
  }

  private def parseHeaderLine(line: String): Option[(String, String)] =
    line.indexOf(':') match {
      case -1  => None
      case idx => Some(line.take(idx).trim -> line.drop(idx + 1).trim)
    }

  private def parseCseq(value: String): Option[CSeq] =
    value.split(" ", 2) match {
      case Array(seqStr, methodStr) => parseCseqParts(seqStr, methodStr)
      case _                        => None
    }

  private def parseCseqParts(seqStr: String, methodStr: String): Option[CSeq] =
    for {
      seq    <- seqStr.toIntOption
      method <- parseMethod(methodStr).toOption
    } yield CSeq(seq, method)

  private def parseMethod(s: String): Either[String, SipMethod] =
    s.trim.toUpperCase match {
      case "INVITE"   => Right(SipMethod.Invite)
      case "ACK"      => Right(SipMethod.Ack)
      case "BYE"      => Right(SipMethod.Bye)
      case "CANCEL"   => Right(SipMethod.Cancel)
      case "OPTIONS"  => Right(SipMethod.Options)
      case "INFO"     => Right(SipMethod.Info)
      case "REGISTER" => Right(SipMethod.Register)
      case other      => Left(s"unknown method: $other")
    }
}
