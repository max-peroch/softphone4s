package softphone4s.codec

import cats.data.NonEmptyList
import cats.syntax.all.*
import fs2.Chunk
import softphone4s.model.*

import java.nio.charset.StandardCharsets.UTF_8
import scala.annotation.tailrec

/** Reads SIP messages from bytes (RFC 3261 §7). The header section is decoded
  * as UTF-8 text; the body is sliced out by `Content-Length`, never scanned.
  */
object SipParser {

  /** The largest datagram UDP can carry. */
  val DefaultMaxSize: Int = 65535

  /** Reads one message from a datagram (or a frame cut by [[frame]]). Blank
    * lines before the start line are ignored. Without `Content-Length` the body
    * is the rest of the datagram.
    */
  def parse(
      bytes: Chunk[Byte],
      maxSize: Int = DefaultMaxSize
  ): Either[ParseError, SipMessage] = {
    val slice = bytes.toArraySlice
    val arr   = slice.values
    val end   = slice.offset + slice.length
    if slice.length > maxSize then
      Left(ParseError.MessageTooLarge(slice.length, maxSize))
    else {
      val start = skipBlank(arr, slice.offset, end)
      if start >= end then Left(ParseError.Empty)
      else {
        val (headerEnd, bodyStart) = findBodyStart(arr, start, end)
        val lines                  =
          splitLines(new String(arr, start, headerEnd - start, UTF_8))
        for {
          fields <- Right(fieldsOf(lines.tail))
          length <- contentLength(fields)
          body   <- bodyOf(arr, bodyStart, end, length)
          msg    <- parseMessage(lines.head, fields, body)
        } yield msg
      }
    }
  }

  /** Finds the first message in a stream buffer, as a TCP or TLS reader must
    * before it can [[parse]] it.
    */
  def frame(buffer: Chunk[Byte], maxSize: Int = DefaultMaxSize): Framing = {
    val slice = buffer.toArraySlice
    val arr   = slice.values
    val end   = slice.offset + slice.length
    val start = skipBlank(arr, slice.offset, end)
    if start > slice.offset then Framing.KeepAlive(start - slice.offset)
    else if start >= end then Framing.NeedMore
    else {
      val (headerEnd, bodyStart) = findBodyStart(arr, start, end)
      if bodyStart >= end && headerEnd >= end then
        if slice.length > maxSize then
          Framing.Invalid(ParseError.MessageTooLarge(slice.length, maxSize))
        else Framing.NeedMore
      else {
        contentLengthOf(arr, start, headerEnd) match {
          case Left(err)   => Framing.Invalid(err)
          case Right(None) => Framing.Invalid(ParseError.MissingContentLength)
          case Right(Some(body)) =>
            val total = (bodyStart - slice.offset) + body.toLong
            if total > maxSize then
              Framing.Invalid(ParseError.MessageTooLarge(total.toInt, maxSize))
            else if slice.length < total then Framing.NeedMore
            else Framing.Frame(total.toInt)
        }
      }
    }
  }

  /** The Content-Length of the header block `arr[start, headerEnd)`, found by
    * scanning bytes where the block is plain, and by decoding it to text
    * otherwise.
    */
  private[codec] def contentLengthOf(
      arr: Array[Byte],
      start: Int,
      headerEnd: Int
  ): Either[ParseError, Option[Int]] =
    scanContentLength(arr, start, headerEnd)
      .getOrElse(contentLengthViaText(arr, start, headerEnd))

  private[codec] def contentLengthViaText(
      arr: Array[Byte],
      start: Int,
      headerEnd: Int
  ): Either[ParseError, Option[Int]] =
    contentLength(
      fieldsOf(
        splitLines(new String(arr, start, headerEnd - start, UTF_8)).tail
      )
    )

  /** `None` when the block has anything the scan does not handle (a folded
    * line, or a value that is not plain digits), so the text path decides.
    */
  private def scanContentLength(
      arr: Array[Byte],
      start: Int,
      headerEnd: Int
  ): Option[Either[ParseError, Option[Int]]] = {

    // The end of the line starting at `from`, before its CR or LF.
    def lineEnd(from: Int): Int = {
      @tailrec def go(i: Int): Int =
        if i >= headerEnd || arr(i) == '\n' then i else go(i + 1)
      val nl = go(from)
      if nl > from && arr(nl - 1) == '\r' then nl - 1 else nl
    }

    def isSpace(b: Byte): Boolean = b == ' ' || b == '\t'

    def lower(b: Byte): Int = if b >= 'A' && b <= 'Z' then b + 32 else b

    def nameIs(from: Int, to: Int, name: String): Boolean =
      to - from == name.length && name.indices.forall(i =>
        lower(arr(from + i)) == name.charAt(i)
      )

    // The digits after the colon of the line [ls, le); `None` if not plain.
    def digits(from: Int, to: Int): Option[Int] = {
      val a = (from until to).find(i => !isSpace(arr(i))).getOrElse(to)
      val b =
        (a until to).reverse.find(i => !isSpace(arr(i))).map(_ + 1).getOrElse(a)
      Option
        .when(
          b > a && b - a <= 9 && (a until b).forall(i =>
            arr(i) >= '0' && arr(i) <= '9'
          )
        )(
          (a until b).foldLeft(0)((n, i) => n * 10 + (arr(i) - '0'))
        )
    }

    @tailrec
    def go(ls: Int): Option[Either[ParseError, Option[Int]]] =
      if ls >= headerEnd then Some(Right(None))
      else {
        val le   = lineEnd(ls)
        val next = (le until headerEnd)
          .find(i => arr(i) == '\n')
          .map(_ + 1)
          .getOrElse(headerEnd)
        if le > ls && isSpace(arr(ls)) then None // folded line
        else {
          val colon = (ls until le).find(i => arr(i) == ':')
          colon match {
            case None    => go(next)
            case Some(c) =>
              val nameEnd = (ls until c).reverse
                .find(i => !isSpace(arr(i)))
                .map(_ + 1)
                .getOrElse(ls)
              if nameIs(ls, nameEnd, "content-length") || nameIs(
                  ls,
                  nameEnd,
                  "l"
                )
              then
                // A folded next line would extend this value.
                if next < headerEnd && isSpace(arr(next)) then None
                else digits(c + 1, le).map(n => Right(Some(n)))
              else go(next)
          }
        }
      }

    // Skip the start line.
    val first     = lineEnd(start)
    val firstNext = (first until headerEnd)
      .find(i => arr(i) == '\n')
      .map(_ + 1)
      .getOrElse(headerEnd)
    go(firstNext)
  }

  // -------------------------------------------------------------------------
  // Bytes
  // -------------------------------------------------------------------------

  @tailrec
  private def skipBlank(arr: Array[Byte], i: Int, end: Int): Int =
    if i < end && (arr(i) == '\r' || arr(i) == '\n') then
      skipBlank(arr, i + 1, end)
    else i

  /** `(end of header text, start of body)`: at the first empty line (CRLFCRLF,
    * or bare-LF line endings); the whole buffer is headers if there is none.
    */
  private def findBodyStart(
      arr: Array[Byte],
      from: Int,
      end: Int
  ): (Int, Int) = {
    @tailrec
    def go(i: Int): (Int, Int) =
      if i >= end then (end, end)
      else if arr(i) != '\n' then go(i + 1)
      else if i + 1 < end && arr(i + 1) == '\n' then (i, i + 2)
      else if i + 2 < end && arr(i + 1) == '\r' && arr(i + 2) == '\n' then
        (i, i + 3)
      else go(i + 1)
    go(from)
  }

  private def bodyOf(
      arr: Array[Byte],
      bodyStart: Int,
      end: Int,
      length: Option[Int]
  ): Either[ParseError, String] = {
    val available = end - bodyStart
    length match {
      case Some(n) if available < n =>
        Left(ParseError.TruncatedBody(n, available))
      case Some(n) => Right(new String(arr, bodyStart, n, UTF_8))
      case None    => Right(new String(arr, bodyStart, available, UTF_8))
    }
  }

  /** The lines of `text`, split on LF with a trailing CR dropped, keeping empty
    * ones (the same as `split("\r?\n", -1)`, without a regex).
    */
  private[codec] def splitLines(text: String): List[String] = {
    @tailrec
    def go(from: Int, acc: List[String]): List[String] =
      text.indexOf('\n', from) match {
        case -1 => (text.substring(from) :: acc).reverse
        case nl =>
          val end =
            if nl > from && text.charAt(nl - 1) == '\r' then nl - 1 else nl
          go(nl + 1, text.substring(from, end) :: acc)
      }
    go(0, Nil)
  }

  // -------------------------------------------------------------------------
  // Header fields
  // -------------------------------------------------------------------------

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
    "proxy-authenticate",
    "expires",
    "min-expires"
  )

  private def canonical(name: String): String = {
    val lower = name.toLowerCase
    CompactForms.getOrElse(lower, lower)
  }

  /** `(name as written, value)` of each header line, with continuation lines
    * (those starting with whitespace) joined onto the preceding one.
    */
  private def fieldsOf(lines: List[String]): List[(String, String)] =
    lines
      .foldLeft(List.empty[String]) {
        case (prev :: rest, line) if line.headOption.exists(_.isWhitespace) =>
          (prev + " " + line.trim) :: rest
        case (acc, line) => line :: acc
      }
      .reverse
      .flatMap { line =>
        line.indexOf(':') match {
          case -1  => None
          case idx => Some(line.take(idx).trim -> line.drop(idx + 1).trim)
        }
      }

  private def contentLength(
      fields: List[(String, String)]
  ): Either[ParseError, Option[Int]] =
    fields
      .collectFirst {
        case (name, value) if canonical(name) == "content-length" => value
      }
      .traverse(text =>
        text.toIntOption
          .filter(_ >= 0)
          .toRight(ParseError.InvalidContentLength(text))
      )

  private def parseHeaders(
      fields: List[(String, String)]
  ): Either[ParseError, SipHeaders] = {
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
    val maxForwards = values("max-forwards").headOption
      .traverse(v =>
        v.trim.toIntOption
          .toRight(s"bad Max-Forwards: '$v'")
          .flatMap(MaxForwards(_))
      )
      .map(_.getOrElse(MaxForwards.Default))
      .toValidatedNel
    val contentType =
      values("content-type").headOption.traverse(MediaType.parse).toValidatedNel
    val expires =
      values("expires").headOption.traverse(Expires.parse).toValidatedNel
    val minExpires =
      values("min-expires").headOption.traverse(Expires.parse).toValidatedNel
    val contacts =
      listValues("contact").traverse(NameAddr.parse).toValidatedNel
    val routes = listValues("route").traverse(NameAddr.parse).toValidatedNel
    val recordRoutes =
      listValues("record-route").traverse(NameAddr.parse).toValidatedNel
    val others = fields
      .filterNot((name, _) => Modelled.contains(canonical(name)))
      .traverse((name, value) => FieldName(name).map(ExtensionHeader(_, value)))
      .toValidatedNel

    (
      callId,
      from,
      to,
      vias,
      cseq,
      maxForwards,
      contentType,
      expires,
      minExpires,
      contacts,
      routes,
      recordRoutes,
      others
    )
      .mapN {
        (
            callId,
            from,
            to,
            vias,
            cseq,
            maxForwards,
            contentType,
            expires,
            minExpires,
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
            maxForwards = maxForwards,
            contentType = contentType,
            expires = expires,
            minExpires = minExpires,
            routes = routes,
            recordRoutes = recordRoutes,
            wwwAuthenticate =
              values("www-authenticate").map(AuthChallenge.parse),
            proxyAuthenticate =
              values("proxy-authenticate").map(AuthChallenge.parse),
            others = others
          )
      }
      .toEither
      .left
      .map(ParseError.BadHeaders(_))
  }

  private def parseCseq(value: String): Option[CSeq] =
    val v   = value.trim
    val gap = v.indexWhere(_.isWhitespace)
    if gap < 0 then None
    else
      for {
        seq    <- v.take(gap).toIntOption.flatMap(CSeqNumber(_).toOption)
        method <- parseMethod(v.drop(gap)).toOption
      } yield CSeq(seq, method)

  // -------------------------------------------------------------------------
  // Start line
  // -------------------------------------------------------------------------

  private def parseMessage(
      startLine: String,
      fields: List[(String, String)],
      body: String
  ): Either[ParseError, SipMessage] =
    if startLine.startsWith("SIP/2.0") then
      for {
        status  <- parseStatusLine(startLine)
        headers <- parseHeaders(fields)
      } yield SipResponse(status._1, status._2, headers, body)
    else
      for {
        request <- parseRequestLine(startLine)
        headers <- parseHeaders(fields)
        // RFC 3261 §8.1.1.5: the CSeq method must match the request line.
        _ <- Either.cond(
          headers.cseq.method == request._1,
          (),
          ParseError.MethodMismatch(request._1, headers.cseq.method)
        )
      } yield SipRequest(request._1, request._2, headers, body)

  private def parseRequestLine(
      line: String
  ): Either[ParseError, (SipMethod, SipUri)] =
    line.split(" ", 3) match {
      case Array(methodText, uriText, "SIP/2.0") =>
        for {
          method <- parseMethod(methodText)
          uri    <- SipUri
            .parse(uriText)
            .left
            .map(ParseError.BadRequestUri(uriText, _))
        } yield (method, uri)
      case _ => Left(ParseError.BadStartLine(line))
    }

  private def parseStatusLine(
      line: String
  ): Either[ParseError, (Int, String)] =
    line.split(" ", 3) match {
      case Array("SIP/2.0", codeText, reason) =>
        codeText.toIntOption
          .filter(code => code >= 100 && code <= 699)
          .toRight(ParseError.BadStatusCode(codeText))
          .map(_ -> reason)
      case Array("SIP/2.0", codeText) =>
        codeText.toIntOption
          .filter(code => code >= 100 && code <= 699)
          .toRight(ParseError.BadStatusCode(codeText))
          .map(_ -> "")
      case _ => Left(ParseError.BadStartLine(line))
    }

  private def parseMethod(s: String): Either[ParseError, SipMethod] =
    s.trim.toUpperCase match {
      case "INVITE"   => Right(SipMethod.Invite)
      case "ACK"      => Right(SipMethod.Ack)
      case "BYE"      => Right(SipMethod.Bye)
      case "CANCEL"   => Right(SipMethod.Cancel)
      case "OPTIONS"  => Right(SipMethod.Options)
      case "INFO"     => Right(SipMethod.Info)
      case "REGISTER" => Right(SipMethod.Register)
      case other      => Left(ParseError.UnknownMethod(other))
    }
}
