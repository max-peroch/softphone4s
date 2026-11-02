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

  private val credentials = Credentials("Mufasa", Secret("Circle Of Life"))

  // RFC 2617 §3.5 test vector
  test("respond with qop=auth matches the RFC 2617 test vector") {
    val challenge = digest(
      """Digest realm="testrealm@host.com", qop="auth,auth-int", nonce="dcd98b7102dd2f0e8b11d0f600bfb0c093", opaque="5ccc069c403ebaf9f0171e9517f40e41""""
    )
    val result = DigestAuth.respond(
      challenge,
      "GET",
      "/dir/index.html",
      "",
      credentials,
      cnonce("0a4f113b"),
      NonceCount.One
    )
    assertEquals(result.response, "6629fae49393a05397450978507c4ef1")
    val text = result.render
    assert(text.contains("""username="Mufasa""""), text)
    assert(text.contains("qop=auth, nc=00000001"), text)
    assert(text.contains("""cnonce="0a4f113b""""), text)
    assert(
      text.contains("""opaque="5ccc069c403ebaf9f0171e9517f40e41""""),
      text
    )
    assert(!text.contains("algorithm"), text)
  }

  // ---- RFC 7616 §3.9.1: the same request under MD5 and SHA-256 --------------

  private val rfc7616Credentials =
    Credentials("Mufasa", Secret("Circle of Life"))
  private val rfc7616Nonce  = "7ypf/xlj9XXwfDPEoM4URrv/xwf94BcCAzFZH4GiTo0v"
  private val rfc7616Cnonce = "f2/wE4q74E6zIJEtWaHKaf5wv/H5QzzpXusqGemxURZJ"

  private def rfc7616(
      algorithm: String,
      qop: String,
      body: String = ""
  ): DigestCredentials =
    DigestAuth.respond(
      digest(
        s"""Digest realm="http-auth@example.org", qop="$qop", algorithm=$algorithm, nonce="$rfc7616Nonce", opaque="FQhe/qaU925kfnzjCev0ciny7QMkPqMAFRtzCUYo5tdS""""
      ),
      "GET",
      "/dir/index.html",
      body,
      rfc7616Credentials,
      cnonce(rfc7616Cnonce),
      NonceCount.One
    )

  test("RFC 7616 example: MD5") {
    assertEquals(
      rfc7616("MD5", "auth").response,
      "8ca523f5e9506fed4657c9700eebdbec"
    )
  }

  test("RFC 7616 example: SHA-256") {
    val result = rfc7616("SHA-256", "auth")
    assertEquals(
      result.response,
      "753927fa0e85d155564e2e272a28d1802ca10daf4496794697cf8db5856cb6c1"
    )
    assertEquals(result.algorithm, DigestAlgorithm.Sha256)
    assert(result.render.contains("algorithm=SHA-256"), result.render)
  }

  // The remaining expected values come from an independent computation
  // (Python's hashlib) over the same inputs.

  test("MD5-sess and SHA-256-sess mix the nonces into the key") {
    assertEquals(
      rfc7616("MD5-sess", "auth").response,
      "e783283f46242139c486a698fec7211d"
    )
    assertEquals(
      rfc7616("SHA-256-sess", "auth").response,
      "2fd51b3a77ad75bad6afad6003e818d767133c46d9e2749e7f5232ae1ea3efd7"
    )
  }

  test("auth-int also hashes the request body") {
    assertEquals(
      rfc7616("MD5", "auth-int", "hello").response,
      "1707eaf5c6ed1a8f6ff2e49286b06bc2"
    )
    assertEquals(
      rfc7616("SHA-256", "auth-int", "hello").response,
      "887bb8d7a6c4cc95528278df0ec68f641244105a1d6a3aa8769488086b20e097"
    )
    assert(
      rfc7616("SHA-256", "auth-int", "hello").render.contains("qop=auth-int")
    )
    assertNotEquals(
      rfc7616("SHA-256", "auth-int", "hello").response,
      rfc7616("SHA-256", "auth-int", "bye").response
    )
  }

  test("auth is preferred to auth-int when both are offered") {
    val result = rfc7616("SHA-256", "auth,auth-int", "hello")
    assertEquals(result.qop.map(_.qop), Some(Qop.Auth))
    assertEquals(result.response, rfc7616("SHA-256", "auth").response)
  }

  test("without qop the response is the RFC 2069 form, for either hash") {
    val md5 = DigestAuth.respond(
      digest(
        s"""Digest realm="http-auth@example.org", nonce="$rfc7616Nonce""""
      ),
      "GET",
      "/dir/index.html",
      "",
      rfc7616Credentials,
      cnonce(rfc7616Cnonce),
      NonceCount.One
    )
    assertEquals(md5.response, "7b2cc3b30e75b4777ea31027084363fd")
    assertEquals(md5.qop, None)
    val sha = DigestAuth.respond(
      digest(
        s"""Digest realm="http-auth@example.org", algorithm=SHA-256, nonce="$rfc7616Nonce""""
      ),
      "GET",
      "/dir/index.html",
      "",
      rfc7616Credentials,
      cnonce(rfc7616Cnonce),
      NonceCount.One
    )
    assertEquals(
      sha.response,
      "a1306b0595a6c7fe96c448631fb5cfbd5107bd1fe1da729d978dd7446b812363"
    )
    assert(!sha.render.contains("qop"), sha.render)
  }

  test("SHA-256 responses are 64 hex digits, MD5 ones 32") {
    assertEquals(rfc7616("SHA-256", "auth").response.length, 64)
    assertEquals(rfc7616("MD5", "auth").response.length, 32)
  }

  test("the response changes with the nonce count") {
    val challenge = digest("""Digest realm="r", nonce="n", qop="auth"""")
    def at(count: NonceCount) = DigestAuth.respond(
      challenge,
      "INVITE",
      "sip:a@b",
      "",
      credentials,
      cnonce("c"),
      count
    )
    assertNotEquals(
      at(NonceCount.One).response,
      at(NonceCount.One.next).response
    )
    assert(at(NonceCount.One.next).render.contains("nc=00000002"))
  }

  test("a challenge is answerable with auth, auth-int, none, or both") {
    List(
      "",
      """, qop="auth"""",
      """, qop="auth-int"""",
      """, qop="auth-int,auth""""
    ).foreach { qop =>
      assert(digest(s"""Digest realm="r", nonce="n"$qop""").isAnswerable, qop)
    }
  }

  test("the strongest algorithm among the answerable challenges is chosen") {
    def challenge(algorithm: String) =
      AuthChallenge.parse(
        s"""Digest realm="r", nonce="$algorithm", algorithm=$algorithm"""
      )
    def chosen(algorithms: String*) =
      AuthChallenge.best(algorithms.toList.map(challenge)).map(_.nonce.value)
    assertEquals(chosen("MD5", "SHA-256"), Some("SHA-256"))
    assertEquals(chosen("SHA-256", "MD5"), Some("SHA-256"))
    assertEquals(chosen("MD5", "SHA-256-sess", "SHA-256"), Some("SHA-256-sess"))
    assertEquals(chosen("MD5"), Some("MD5"))
    assertEquals(
      chosen("SHA-512-256", "MD5"),
      Some("MD5")
    ) // not supported, so skipped
    assertEquals(chosen("SHA-512-256"), None)
    assertEquals(AuthChallenge.best(Nil), None)
  }
}
