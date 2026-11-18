package softphone4s.model

import cats.syntax.all.*

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

/** Quote- and bracket-aware splitting and header-parameter handling shared by
  * the typed header values.
  */
private[softphone4s] object HeaderSyntax {

  /** Splits `s` on `sep`, ignoring separators inside quoted strings and `<>`.
    */
  def split(s: String, sep: Char): List[String] = {
    @tailrec
    def go(
        i: Int,
        start: Int,
        quoted: Boolean,
        angled: Boolean,
        acc: List[String]
    ): List[String] =
      if i >= s.length then (s.substring(start) :: acc).reverse
      else
        s(i) match {
          case '\\' if quoted => go(i + 2, start, quoted, angled, acc)
          case '"' if !angled => go(i + 1, start, !quoted, angled, acc)
          case '<' if !quoted => go(i + 1, start, quoted, true, acc)
          case '>' if !quoted => go(i + 1, start, quoted, false, acc)
          case c if c == sep && !quoted && !angled =>
            go(i + 1, i + 1, quoted, angled, s.substring(start, i) :: acc)
          case _ => go(i + 1, start, quoted, angled, acc)
        }
    go(0, 0, quoted = false, angled = false, Nil)
  }

  /** Index of the first `c` that is outside a quoted string. */
  def firstUnquoted(s: String, c: Char): Option[Int] = {
    @tailrec
    def go(i: Int, quoted: Boolean): Option[Int] =
      if i >= s.length then None
      else
        s(i) match {
          case '\\' if quoted         => go(i + 2, quoted)
          case '"'                    => go(i + 1, !quoted)
          case x if x == c && !quoted => Some(i)
          case _                      => go(i + 1, quoted)
        }
    go(0, quoted = false)
  }

  /** Parses `;name[=value]` parameters (without the leading `;`); a repeated
    * name is an error.
    */
  def parseParams(text: String): Either[String, HeaderParams] =
    split(text, ';')
      .map(_.trim)
      .filter(_.nonEmpty)
      .traverse { segment =>
        val (name, value) = segment.indexOf('=') match {
          case -1  => (segment, None)
          case idx => (segment.take(idx).trim, Some(segment.drop(idx + 1).trim))
        }
        (ParamName(name), value.traverse(HeaderParamValue(_))).tupled
      }
      .flatMap { entries =>
        val names = entries.map(_._1)
        Either.cond(
          names.distinct.size == names.size,
          ListMap.from(entries),
          "a header parameter appears more than once"
        )
      }

  def renderParams(params: HeaderParams): String =
    params.map {
      case (n, Some(v)) => s";${n.value}=${v.value}"
      case (n, None)    => s";${n.value}"
    }.mkString
}
