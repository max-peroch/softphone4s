package softphone4s.model

import munit.FunSuite

class AuthChallengeSuite extends FunSuite {

  test("parses a Digest challenge with every parameter") {
    AuthChallenge.parse(
      """Digest realm="sip.example.com", nonce="abc", opaque="xyz", algorithm=MD5-sess, qop="auth,auth-int", stale=false"""
    ) match {
      case AuthChallenge.Digest(c) =>
        assertEquals(c.realm, "sip.example.com")
        assertEquals(c.nonce.value, "abc")
        assertEquals(c.opaque, Some("xyz"))
        assertEquals(c.algorithm, DigestAlgorithm.Md5Sess)
        assertEquals(c.qop, Set(Qop.Auth, Qop.AuthInt))
      case other => fail(s"unexpected: $other")
    }
  }

  test("the scheme is case-insensitive and the algorithm defaults to MD5") {
    AuthChallenge.parse("""DIGEST realm="r", nonce="n"""") match {
      case AuthChallenge.Digest(c) =>
        assertEquals(c.algorithm, DigestAlgorithm.Md5)
        assertEquals(c.qop, Set.empty[Qop])
      case other => fail(s"unexpected: $other")
    }
  }

  test("commas and escaped quotes inside quoted values are kept") {
    AuthChallenge.parse("""Digest realm="a, \"b\"", nonce="n"""") match {
      case AuthChallenge.Digest(c) => assertEquals(c.realm, "a, \"b\"")
      case other                   => fail(s"unexpected: $other")
    }
  }

  test(
    "other schemes, unknown algorithms and malformed Digests are Unsupported"
  ) {
    List(
      "Negotiate abc123",
      "Basic realm=\"x\"",
      """Digest realm="r", nonce="n", algorithm=SHA-512-256""",
      """Digest realm="r"""",
      """Digest nonce="n"""",
      """Digest realm="r", nonce="""",
      ""
    ).foreach { header =>
      AuthChallenge.parse(header) match {
        case AuthChallenge.Unsupported(_, _) => ()
        case other => fail(s"expected Unsupported for '$header', got $other")
      }
    }
  }

  test("render round-trips a Digest challenge") {
    val challenge =
      """Digest realm="r\"x", nonce="n", opaque="o", algorithm=MD5, qop="auth,auth-int""""
    val parsed = AuthChallenge.parse(challenge)
    assertEquals(AuthChallenge.parse(parsed.render), parsed)
  }

  test("stale=true is read case-insensitively and rendered back") {
    List("stale=true", "stale=TRUE", "stale=\"true\"").foreach { param =>
      AuthChallenge.parse(s"""Digest realm="r", nonce="n", $param""") match {
        case AuthChallenge.Digest(c) =>
          assertEquals(c.staleness, Staleness.Stale, param)
          assert(c.render.contains("stale=true"), c.render)
        case other => fail(s"unexpected: $other")
      }
    }
  }

  test("a challenge without stale, or with stale=false, is fresh") {
    List("", ", stale=false", ", stale=maybe").foreach { param =>
      AuthChallenge.parse(s"""Digest realm="r", nonce="n"$param""") match {
        case AuthChallenge.Digest(c) =>
          assertEquals(c.staleness, Staleness.Fresh, param)
          assert(!c.render.contains("stale"), c.render)
        case other => fail(s"unexpected: $other")
      }
    }
  }

  test("SHA-256 and SHA-256-sess are Digest challenges we can answer") {
    List(
      "SHA-256"      -> DigestAlgorithm.Sha256,
      "sha-256"      -> DigestAlgorithm.Sha256,
      "SHA-256-sess" -> DigestAlgorithm.Sha256Sess
    ).foreach { (token, algorithm) =>
      AuthChallenge.parse(
        s"""Digest realm="r", nonce="n", algorithm=$token"""
      ) match {
        case AuthChallenge.Digest(c) =>
          assertEquals(c.algorithm, algorithm, token)
        case other => fail(s"unexpected for $token: $other")
      }
    }
  }

  test(
    "SHA-256 is stronger than MD5, and a -sess form is as strong as its base"
  ) {
    assert(DigestAlgorithm.Sha256.strength > DigestAlgorithm.Md5.strength)
    assertEquals(
      DigestAlgorithm.Sha256Sess.strength,
      DigestAlgorithm.Sha256.strength
    )
    assertEquals(DigestAlgorithm.Md5Sess.strength, DigestAlgorithm.Md5.strength)
    assert(
      DigestAlgorithm.Sha256Sess.session && !DigestAlgorithm.Sha256.session
    )
  }
}
