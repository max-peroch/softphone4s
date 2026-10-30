package softphone4s.codec

import munit.FunSuite
import softphone4s.model.*

class DigestAuthSuite extends FunSuite {

  private def digest(header: String): DigestChallenge =
    AuthChallenge.parse(header) match {
      case AuthChallenge.Digest(c) => c
      case other => fail(s"expected a Digest challenge, got $other")
    }

  private def cnonce(s: String): Cnonce =
    Cnonce(s).fold(err => fail(err), identity)

  private val credentials = Credentials("Mufasa", "Circle Of Life")

  // RFC 2617 §3.5 test vector
  test("respond with qop=auth matches the RFC 2617 test vector") {
    val challenge = digest(
      """Digest realm="testrealm@host.com", qop="auth,auth-int", nonce="dcd98b7102dd2f0e8b11d0f600bfb0c093", opaque="5ccc069c403ebaf9f0171e9517f40e41""""
    )
    val result = DigestAuth.respond(
      challenge,
      "GET",
      "/dir/index.html",
      credentials,
      cnonce("0a4f113b"),
      NonceCount.One
    )
    assertEquals(result.response, "6629fae49393a05397450978507c4ef1")
    val text = result.render
    assert(text.contains("""username="Mufasa""""), text)
    assert(text.contains("qop=auth, nc=00000001"), text)
    assert(text.contains("""cnonce="0a4f113b""""), text)
    assert(text.contains("""opaque="5ccc069c403ebaf9f0171e9517f40e41""""), text)
    assert(!text.contains("algorithm"), text)
  }

  test("without qop the response is the RFC 2069 form: no qop, nc or cnonce") {
    val result = DigestAuth.respond(
      digest("""Digest realm="example.com", nonce="simplenonce""""),
      "INVITE",
      "sip:callee@example.com",
      Credentials("user", "password"),
      cnonce("ignored"),
      NonceCount.One
    )
    val text = result.render
    assertEquals(result.qop, None)
    assert(!text.contains("qop"), text)
    assert(!text.contains("nc="), text)
    assert(!text.contains("cnonce"), text)
    assert(text.contains("""realm="example.com""""), text)
  }

  test("MD5-sess mixes the cnonce into HA1 and names the algorithm") {
    val challenge = digest(
      """Digest realm="r", nonce="n", algorithm=MD5-sess, qop="auth""""
    )
    val a = DigestAuth.respond(
      challenge,
      "INVITE",
      "sip:a@b",
      credentials,
      cnonce("c1"),
      NonceCount.One
    )
    val b = DigestAuth.respond(
      challenge,
      "INVITE",
      "sip:a@b",
      credentials,
      cnonce("c2"),
      NonceCount.One
    )
    assertNotEquals(a.response, b.response)
    assert(a.render.contains("algorithm=MD5-sess"), a.render)
  }

  test("the response changes with the nonce count") {
    val challenge = digest("""Digest realm="r", nonce="n", qop="auth"""")
    val one       = DigestAuth.respond(
      challenge,
      "INVITE",
      "sip:a@b",
      credentials,
      cnonce("c"),
      NonceCount.One
    )
    val two = DigestAuth.respond(
      challenge,
      "INVITE",
      "sip:a@b",
      credentials,
      cnonce("c"),
      NonceCount.One.next
    )
    assertNotEquals(one.response, two.response)
    assert(two.render.contains("nc=00000002"), two.render)
  }

  test("a challenge that offers only auth-int is not answerable") {
    assert(
      !digest("""Digest realm="r", nonce="n", qop="auth-int"""").isAnswerable
    )
    assert(
      digest(
        """Digest realm="r", nonce="n", qop="auth-int,auth""""
      ).isAnswerable
    )
    assert(digest("""Digest realm="r", nonce="n"""").isAnswerable)
  }
}
