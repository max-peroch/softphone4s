package softphone4s.model

import scala.annotation.tailrec

/** Character classes of RFC 3261 §25.1, matched by scanning rather than with
  * regexes, which dominated parsing time.
  */
private[softphone4s] object UriSyntax {
  private val Unreserved = CharSet.AlphaNumeric + "-_.!~*'()"

  /** RFC 3261 `token` characters. */
  val TokenChars: CharSet = CharSet.AlphaNumeric + "-.!%*_+`'~"

  /** `word` characters of a Call-ID: token plus ()<>:\/[]?{}@ */
  val CallIdChars: CharSet = TokenChars + "()<>:\\/[]?{}@"

  val UserChars: CharSet     = Unreserved + "&=+$,;/"
  val PasswordChars: CharSet = Unreserved + "&=+$,"
  val ParamChars: CharSet    = Unreserved + "[]/:&+$"
  // '@' is not in the grammar but is common in header values; tolerated.
  val HeaderChars: CharSet = Unreserved + "[]/?:+$@"

  /** What a header parameter value may be unquoted: a token or a host. */
  val ParamValueChars: CharSet = TokenChars + "[]:"

  /** Text made of characters from a set, or `%HH` escapes. */
  final class Syntax(
      chars: CharSet,
      emptiness: Emptiness,
      escapes: Escapes
  ) {

    def matches(s: String): Boolean = {
      val n = s.length

      @tailrec
      def go(i: Int): Boolean =
        if i >= n then true
        else {
          val c = s.charAt(i)
          if chars.contains(c) then go(i + 1)
          else if escapes == Escapes.Percent && c == '%' && i + 2 < n && isHex(
              s.charAt(i + 1)
            ) && isHex(s.charAt(i + 2))
          then go(i + 3)
          else false
        }

      if n == 0 then emptiness == Emptiness.Allowed else go(0)
    }
  }

  /** One or more (or zero or more) characters of `chars`, or `%HH` escapes. */
  def token(chars: CharSet, emptiness: Emptiness): Syntax =
    new Syntax(chars, emptiness, Escapes.Percent)

  /** A quoted-string: text between double quotes with `\x` escapes. */
  def isQuotedString(s: String): Boolean = {
    val last = s.length - 1

    @tailrec
    def go(i: Int): Boolean =
      if i == last then true
      else if i > last then false
      else
        s.charAt(i) match {
          case '"'  => false
          case '\\' => i + 1 < last && s.charAt(i + 1) != '\n' && go(i + 2)
          case _    => go(i + 1)
        }

    s.length >= 2 && s.charAt(0) == '"' && s.charAt(last) == '"' && go(1)
  }

  /** Removes the backslash from each `\x` escape. */
  def unescape(s: String): String =
    if s.indexOf('\\') < 0 then s
    else {
      val out = new java.lang.StringBuilder(s.length)
      @tailrec
      def go(i: Int): Unit =
        if i < s.length then {
          val c = s.charAt(i)
          if c == '\\' && i + 1 < s.length then {
            out.append(s.charAt(i + 1)); go(i + 2)
          } else { out.append(c); go(i + 1) }
        }
      go(0)
      out.toString
    }

  private def isHex(c: Char): Boolean =
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

  private val HexDigits = "0123456789abcdef".toCharArray

  /** Lower-case hex of `bytes`. */
  def hex(bytes: Array[Byte]): String = {
    val out = new Array[Char](bytes.length * 2)
    var i   = 0
    while i < bytes.length do {
      out(2 * i) = HexDigits((bytes(i) >> 4) & 0xf)
      out(2 * i + 1) = HexDigits(bytes(i) & 0xf)
      i += 1
    }
    new String(out)
  }

  def check(what: String, syntax: Syntax, s: String): Either[String, String] =
    Either.cond(syntax.matches(s), s, s"invalid $what: '$s'")
}
