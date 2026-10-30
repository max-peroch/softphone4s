package softphone4s.codec

import fs2.Chunk
import softphone4s.model.*

import java.nio.charset.StandardCharsets.UTF_8

/** Pure SIP codec between messages and bytes — no side effects. */
object SipCodec {

  private val CRLF = "\r\n"

  /** The wire form of `msg`: header section as UTF-8, then the body bytes, with
    * `Content-Length` set to the body's length in bytes.
    */
  def encode(msg: SipMessage): Chunk[Byte] = {
    val (startLine, headers, body) = msg match {
      case r: SipRequest =>
        (
          s"${r.method.toString.toUpperCase} ${r.requestUri.render} SIP/2.0",
          r.headers,
          r.body
        )
      case r: SipResponse =>
        (s"SIP/2.0 ${r.statusCode} ${r.reason}", r.headers, r.body)
    }
    val bodyBytes = body.getBytes(UTF_8)
    val head      =
      (startLine + CRLF + encodeHeaders(headers, bodyBytes.length) + CRLF)
        .getBytes(UTF_8)
    val out = new Array[Byte](head.length + bodyBytes.length)
    System.arraycopy(head, 0, out, 0, head.length)
    System.arraycopy(bodyBytes, 0, out, head.length, bodyBytes.length)
    Chunk.array(out)
  }

  /** The wire form of `msg` as text, for logs and tests. */
  def render(msg: SipMessage): String =
    new String(encode(msg).toArray, UTF_8)

  private def encodeHeaders(headers: SipHeaders, bodyLength: Int): String = {
    val out = new StringBuilder
    headers.vias.toList.foreach(via => out.append(s"Via: ${via.render}$CRLF"))
    out.append(s"From: ${headers.from.render}$CRLF")
    out.append(s"To: ${headers.to.render}$CRLF")
    out.append(s"Call-ID: ${headers.callId.value}$CRLF")
    out.append(
      s"CSeq: ${headers.cseq.seq.value} ${headers.cseq.method.toString.toUpperCase}$CRLF"
    )
    headers.contacts.foreach(c => out.append(s"Contact: ${c.render}$CRLF"))
    out.append(s"Max-Forwards: ${headers.maxForwards.value}$CRLF")
    headers.routes.foreach(r => out.append(s"Route: ${r.render}$CRLF"))
    headers.recordRoutes.foreach(r =>
      out.append(s"Record-Route: ${r.render}$CRLF")
    )
    headers.wwwAuthenticate.foreach(c =>
      out.append(s"WWW-Authenticate: ${c.render}$CRLF")
    )
    headers.proxyAuthenticate.foreach(c =>
      out.append(s"Proxy-Authenticate: ${c.render}$CRLF")
    )
    headers.contentType.foreach(contentType =>
      out.append(s"Content-Type: ${contentType.render}$CRLF")
    )
    out.append(s"Content-Length: $bodyLength$CRLF")
    headers.others.foreach(h => out.append(s"${h.name.value}: ${h.value}$CRLF"))
    out.toString
  }

  def decode(bytes: Chunk[Byte]): Either[ParseError, SipMessage] =
    SipParser.parse(bytes)

  def decode(raw: String): Either[ParseError, SipMessage] =
    decode(Chunk.array(raw.getBytes(UTF_8)))
}
