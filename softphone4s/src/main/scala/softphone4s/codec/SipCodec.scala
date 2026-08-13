package softphone4s.codec

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
      s"${r.method.toString.toUpperCase} ${r.requestUri} SIP/2.0"
    s"$requestLine$CRLF${encodeHeaders(r.headers, r.body)}$CRLF${r.body}"
  }

  private def encodeResponse(r: SipResponse): String = {
    val statusLine = s"SIP/2.0 ${r.statusCode} ${r.reason}"
    s"$statusLine$CRLF${encodeHeaders(r.headers, r.body)}$CRLF${r.body}"
  }

  private val DefaultMaxForwards = 70

  private def encodeHeaders(headers: SipHeaders, body: String): String = {
    val out = new StringBuilder
    out.append(s"Via: SIP/2.0/UDP ${headers.via}$CRLF")
    out.append(s"From: ${headers.from}$CRLF")
    out.append(s"To: ${headers.to}$CRLF")
    out.append(s"Call-ID: ${headers.callId}$CRLF")
    out.append(
      s"CSeq: ${headers.cseq.seq} ${headers.cseq.method.toString.toUpperCase}$CRLF"
    )
    out.append(s"Contact: ${headers.contact}$CRLF")
    out.append(s"Max-Forwards: ${headers.maxForwards}$CRLF")
    headers.route.foreach(route => out.append(s"Route: $route$CRLF"))
    headers.contentType.foreach(contentType =>
      out.append(s"Content-Type: $contentType$CRLF")
    )
    val bodyLength = body.getBytes("UTF-8").length
    out.append(s"Content-Length: $bodyLength$CRLF")
    headers.extraHeaders.foreach { case (name, values) =>
      values.foreach(value => out.append(s"$name: $value$CRLF"))
    }
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
      parseHeaders(headerLines, CSeq(1, method))
        .map(headers =>
          SipRequest(method, uri, headers, bodyLines.mkString(CRLF))
        )
    }

  private def parseRequestLine(
      line: String
  ): Either[String, (SipMethod, String)] =
    line.split(" ", 3) match {
      case Array(methodStr, uri, "SIP/2.0") =>
        parseMethod(methodStr).map(_ -> uri)
      case _ => Left(s"bad request line: $line")
    }

  private def parseResponse(
      statusLine: String,
      rest: List[String]
  ): Either[String, SipMessage] =
    parseStatusLine(statusLine).flatMap { (code, reason) =>
      val (headerLines, bodyLines) = splitHeadersBody(rest)
      parseHeaders(headerLines, CSeq(0, SipMethod.Invite))
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

  private def parseHeaders(
      lines: List[String],
      defaultCseq: CSeq
  ): Either[String, SipHeaders] = {
    // Unfold multi-line headers (continuation lines start with whitespace).
    // foldLeft so continuations are appended to the preceding line, not the following one.
    val unfolded = lines
      .foldLeft(List.empty[String]) {
        case (prev :: rest, line) if line.headOption.exists(_.isWhitespace) =>
          (prev + " " + line.trim) :: rest
        case (acc, line) => line :: acc
      }
      .reverse

    val pairs = unfolded.flatMap(parseHeaderLine)

    val headersByName = pairs.groupBy(_._1).view.mapValues(_.map(_._2)).toMap

    def firstValue(name: String) = headersByName.getOrElse(name, Nil).headOption
    def allValues(name: String)  = headersByName.getOrElse(name, Nil)

    val cseq = firstValue("cseq").flatMap(parseCseq).getOrElse(defaultCseq)

    Right(
      SipHeaders(
        callId = firstValue("call-id").getOrElse(""),
        from = firstValue("from").orElse(firstValue("f")).getOrElse(""),
        to = firstValue("to").orElse(firstValue("t")).getOrElse(""),
        via = firstValue("via").orElse(firstValue("v")).getOrElse(""),
        cseq = cseq,
        contact = firstValue("contact").orElse(firstValue("m")).getOrElse(""),
        maxForwards = firstValue("max-forwards")
          .flatMap(_.toIntOption)
          .getOrElse(DefaultMaxForwards),
        contentType = firstValue("content-type"),
        contentLength =
          firstValue("content-length").flatMap(_.toIntOption).getOrElse(0),
        route = allValues("route"),
        recordRoute = allValues("record-route"),
        wwwAuthenticate = firstValue("www-authenticate"),
        proxyAuthenticate = firstValue("proxy-authenticate"),
        extraHeaders = headersByName -- Set(
          "call-id",
          "from",
          "f",
          "to",
          "t",
          "via",
          "v",
          "cseq",
          "contact",
          "m",
          "max-forwards",
          "content-type",
          "content-length",
          "route",
          "record-route",
          "www-authenticate",
          "proxy-authenticate"
        )
      )
    )
  }

  private def parseHeaderLine(line: String): Option[(String, String)] =
    line.indexOf(':') match {
      case -1  => None
      case idx =>
        Some(line.take(idx).trim.toLowerCase -> line.drop(idx + 1).trim)
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
