package softphone4s.model

import com.comcast.ip4s.Host
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

/** `HostPort.parseHost` short-cuts `Host.fromString`; it must agree with it. */
class HostPortSuite extends ScalaCheckSuite {

  // These guard rewrites that must match the old behaviour exactly.
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(2000)

  private val hostLike: Gen[String] =
    Gen
      .choose(0, 16)
      .flatMap(n =>
        Gen
          .listOfN(
            n,
            Gen.frequency(
              6 -> Gen.oneOf("0123456789".toList),
              4 -> Gen.const('.'),
              3 -> Gen.oneOf("abcxyzAB-".toList),
              1 -> Gen.const(':'),
              1 -> Gen.oneOf("é_ ".toList)
            )
          )
          .map(_.mkString)
      )

  property("agrees with Host.fromString on arbitrary host-like text") {
    forAll(hostLike)(s =>
      assertEquals(HostPort.parseHost(s), Host.fromString(s))
    )
  }

  property("agrees on dotted quads, in range and out") {
    val quad = Gen.listOfN(4, Gen.choose(0, 300)).map(_.mkString("."))
    forAll(quad)(s => assertEquals(HostPort.parseHost(s), Host.fromString(s)))
  }

  test("known cases") {
    List(
      "1.2.3.4",
      "192.168.0.1",
      "256.1.1.1",
      "1.2.3",
      "01.02.03.04",
      "sip.example.com",
      "EXAMPLE.com",
      "2001:db8::1",
      "::1",
      "a",
      "1a.b",
      "-bad.com",
      "",
      ".",
      "1.2.3.4.5"
    )
      .foreach(s => assertEquals(HostPort.parseHost(s), Host.fromString(s), s))
  }
}
