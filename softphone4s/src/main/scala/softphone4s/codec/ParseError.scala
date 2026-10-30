package softphone4s.codec

import cats.data.NonEmptyList

/** Why a datagram or stream frame could not be read as a SIP message. */
enum ParseError {
  case Empty
  case MessageTooLarge(size: Int, limit: Int)
  case BadStartLine(line: String)
  case UnknownMethod(method: String)
  case BadRequestUri(uri: String, reason: String)
  case BadStatusCode(text: String)
  case BadHeaders(errors: NonEmptyList[String])
  case InvalidContentLength(text: String)
  case TruncatedBody(expected: Int, actual: Int)

  /** Only when framing a stream, where `Content-Length` is mandatory. */
  case MissingContentLength

  def message: String = this match {
    case Empty                      => "empty message"
    case MessageTooLarge(size, max) => s"message of $size bytes exceeds $max"
    case BadStartLine(line)         => s"bad start line: '$line'"
    case UnknownMethod(method)      => s"unknown method: $method"
    case BadRequestUri(uri, reason) => s"bad request-URI '$uri': $reason"
    case BadStatusCode(text)        => s"bad status code: '$text'"
    case BadHeaders(errors)         => errors.toList.mkString("; ")
    case InvalidContentLength(text) => s"invalid Content-Length: '$text'"
    case TruncatedBody(expected, actual) =>
      s"body is $actual bytes but Content-Length says $expected"
    case MissingContentLength => "missing Content-Length header"
  }
}
