package softphone4s.codec

import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

/** `splitLines` replaced a regex split for speed; hold it to the same result.
  */
class SipLineSplittingSuite extends ScalaCheckSuite {

  // These guard rewrites that must match the old behaviour exactly.
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(2000)

  property("splitLines agrees with splitting on an optional CR then LF") {
    val lines = Gen
      .choose(0, 14)
      .flatMap(n =>
        Gen.listOfN(n, Gen.oneOf("ab\r\n :".toList)).map(_.mkString)
      )
    forAll(lines)(s =>
      assertEquals(SipParser.splitLines(s).toList, s.split("\r?\n", -1).toList)
    )
  }

  // A header block assembled from lines that include the awkward cases: odd
  // spacing and case, the compact form, folding, signs, and non-digits.
  private val headerLine: Gen[String] = Gen.oneOf(
    "Content-Length: 12",
    "content-length:7",
    "CONTENT-LENGTH :  42 ",
    "Content-Length:\t3",
    "l: 5",
    "L : 9",
    "Content-Length: 12abc",
    "Content-Length: -1",
    "Content-Length: +4",
    "Content-Length: ",
    "Content-Length: 99999999999",
    "Content-Length: 0012",
    "Content-Lengthy: 5",
    "X-Content-Length: 5",
    "Subject: l: 5",
    "Via: SIP/2.0/UDP h",
    " folded continuation",
    "\tmore folding",
    "No colon here",
    "Call-ID: x",
    "Content-Length: 1 2"
  )

  property("the byte scan for Content-Length agrees with the text path") {
    val block = Gen
      .choose(0, 6)
      .flatMap(n =>
        Gen
          .listOfN(n, headerLine)
          .map(ls => ("INVITE sip:a@b SIP/2.0" :: ls).mkString("\r\n"))
      )
    forAll(block) { text =>
      val arr = text.getBytes(java.nio.charset.StandardCharsets.UTF_8)
      assertEquals(
        SipParser.contentLengthOf(arr, 0, arr.length),
        SipParser.contentLengthViaText(arr, 0, arr.length)
      )
    }
  }
}
