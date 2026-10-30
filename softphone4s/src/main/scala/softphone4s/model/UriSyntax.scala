package softphone4s.model

import scala.util.matching.Regex

/** Character classes of RFC 3261 §25.1, as regex character-class bodies. */
private[model] object UriSyntax {
  private val Unreserved = "A-Za-z0-9\\-_.!~*'()"

  /** RFC 3261 `token` characters. */
  val TokenChars: String = "A-Za-z0-9\\-.!%*_+`'~"
  // `word` characters of a Call-ID: token plus ()<>:\/[]?{}@
  val CallIdChars: String = TokenChars + "()<>:\\\\/\\[\\]?{}@"

  val UserChars: String     = Unreserved + "&=+$,;/"
  val PasswordChars: String = Unreserved + "&=+$,"
  val ParamChars: String    = Unreserved + "\\[\\]/:&+$"
  // '@' is not in the grammar but is common in header values; tolerated.
  val HeaderChars: String = Unreserved + "\\[\\]/?:+$@"

  /** One or more (or zero or more) characters of `chars`, or `%HH` escapes. */
  def token(chars: String, nonEmpty: Boolean): Regex =
    s"(?:[$chars]|%[0-9A-Fa-f]{2})${if nonEmpty then "+" else "*"}".r

  /** Lower-case hex of `bytes`. */
  def hex(bytes: Array[Byte]): String = bytes.map("%02x".format(_)).mkString

  def check(what: String, syntax: Regex, s: String): Either[String, String] =
    Either.cond(syntax.matches(s), s, s"invalid $what: '$s'")
}
