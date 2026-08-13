package softphone4s.codec

import munit.FunSuite

class DigestAuthSuite extends FunSuite {

  // RFC 2617 Appendix A test vector
  test("compute with qop=auth matches RFC 2617 test vector") {
    val challenge =
      """Digest realm="testrealm@host.com", qop="auth", nonce="dcd98b7102dd2f0e8b11d0f600bfb0c093""""
    val result = DigestAuth.compute(
      method = "GET",
      requestUri = "/dir/index.html",
      username = "Mufasa",
      password = "Circle Of Life",
      challengeHeader = challenge,
      cnonce = "0a4f113b",
      nc = 1
    )
    assert(
      result.contains("""response="6629fae49393a05397450978507c4ef1""""),
      s"got: $result"
    )
    assert(result.contains("qop=auth"))
    assert(result.contains("nc=00000001"))
    assert(result.contains("""cnonce="0a4f113b""""))
  }

  test(
    "compute without qop produces basic digest — no qop, nc, or cnonce in output"
  ) {
    val challenge = """Digest realm="example.com", nonce="simplenonce""""
    val result    = DigestAuth.compute(
      method = "INVITE",
      requestUri = "sip:callee@example.com",
      username = "user",
      password = "password",
      challengeHeader = challenge,
      cnonce = "ignored"
    )
    assert(result.startsWith("Digest"), s"got: $result")
    assert(result.contains("""username="user""""))
    assert(result.contains("""realm="example.com""""))
    assert(result.contains("""response=""""))
    assert(!result.contains("qop"), s"should not contain qop: $result")
    assert(!result.contains("nc="), s"should not contain nc: $result")
  }

  test("compute with opaque includes opaque in result") {
    val challenge = """Digest realm="example.com", nonce="abc", opaque="xyz""""
    val result    = DigestAuth.compute(
      method = "INVITE",
      requestUri = "sip:callee@example.com",
      username = "user",
      password = "pass",
      challengeHeader = challenge,
      cnonce = "cnonce"
    )
    assert(result.contains("""opaque="xyz""""), s"got: $result")
  }

  test("extractNonce returns the nonce from a challenge header") {
    val challenge =
      """Digest realm="example.com", nonce="mynonce123", algorithm=MD5"""
    assertEquals(DigestAuth.extractNonce(challenge), Some("mynonce123"))
  }

  test("extractNonce returns None when nonce is absent") {
    val challenge = """Digest realm="example.com", algorithm=MD5"""
    assertEquals(DigestAuth.extractNonce(challenge), None)
  }
}
