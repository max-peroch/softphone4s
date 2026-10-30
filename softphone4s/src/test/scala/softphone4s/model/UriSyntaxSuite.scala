package softphone4s.model

import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

/** The scanners in `UriSyntax` replaced regexes for speed; these properties
  * hold them to the regexes' exact behaviour.
  */
class UriSyntaxSuite extends ScalaCheckSuite {

  // These guard rewrites that must match the old behaviour exactly.
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(2000)

  private val unreserved = "A-Za-z0-9\\-_.!~*'()"
  private val tokenBody  = "A-Za-z0-9\\-.!%*_+`'~"

  private val classes: List[(String, String, UriSyntax.Syntax, Boolean)] = List(
    (
      "token",
      tokenBody,
      UriSyntax.token(UriSyntax.TokenChars, nonEmpty = true),
      true
    ),
    (
      "callId",
      tokenBody + "()<>:\\\\/\\[\\]?{}@",
      UriSyntax.token(UriSyntax.CallIdChars, nonEmpty = true),
      true
    ),
    (
      "user",
      unreserved + "&=+$,;/",
      UriSyntax.token(UriSyntax.UserChars, nonEmpty = true),
      true
    ),
    (
      "password",
      unreserved + "&=+$,",
      UriSyntax.token(UriSyntax.PasswordChars, nonEmpty = false),
      false
    ),
    (
      "param",
      unreserved + "\\[\\]/:&+$",
      UriSyntax.token(UriSyntax.ParamChars, nonEmpty = true),
      true
    ),
    (
      "header",
      unreserved + "\\[\\]/?:+$@",
      UriSyntax.token(UriSyntax.HeaderChars, nonEmpty = false),
      false
    )
  )

  // Every character any class mentions, hex digits for escapes, and ones that
  // are never allowed (space, quote, non-ASCII).
  private val alphabet =
    ("abcXYZ019AFaf-_.!~*'()%&=+$,;/[]?:@<>\\{} \"é`" + "\u0000\u007f").toList

  private val text: Gen[String] =
    Gen
      .choose(0, 14)
      .flatMap(n => Gen.listOfN(n, Gen.oneOf(alphabet)).map(_.mkString))

  classes.foreach { (name, body, syntax, nonEmpty) =>
    val oracle =
      s"(?:[$body]|%[0-9A-Fa-f]{2})${if nonEmpty then "+" else "*"}".r
    property(s"$name syntax agrees with its regex") {
      forAll(text)(s => assertEquals(syntax.matches(s), oracle.matches(s)))
    }
  }

  property("header parameter values agree with the regex they replaced") {
    val oracle =
      s"""(?:[$tokenBody\\[\\]:]+|"(?:[^"\\\\]|\\\\.)*")""".r
    forAll(text)(s =>
      assertEquals(HeaderParamValue(s).isRight, oracle.matches(s))
    )
  }

  property("quoted strings with escapes agree with the regex") {
    val quoted = Gen
      .choose(0, 10)
      .flatMap(n =>
        Gen
          .listOfN(n, Gen.oneOf("ab;,\\\" ".toList))
          .map(_.mkString("\"", "", "\""))
      )
    val oracle = """"(?:[^"\\]|\\.)*"""".r
    forAll(quoted)(s =>
      assertEquals(UriSyntax.isQuotedString(s), oracle.matches(s))
    )
  }

  property("unescape agrees with removing the backslash of each \\x") {
    val withBackslashes = Gen
      .choose(0, 12)
      .flatMap(n => Gen.listOfN(n, Gen.oneOf("ab\\\" ".toList)).map(_.mkString))
    forAll(withBackslashes)(s =>
      assertEquals(UriSyntax.unescape(s), s.replaceAll("""\\(.)""", "$1"))
    )
  }

  property("hex agrees with formatting each byte") {
    forAll(Gen.listOf(Gen.choose(Byte.MinValue, Byte.MaxValue))) { bytes =>
      val array = bytes.toArray
      assertEquals(UriSyntax.hex(array), array.map("%02x".format(_)).mkString)
    }
  }
}
