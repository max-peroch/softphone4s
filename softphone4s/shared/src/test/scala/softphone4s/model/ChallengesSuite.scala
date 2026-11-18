package softphone4s.model

import cats.data.NonEmptyList
import munit.FunSuite
import softphone4s.SipLiterals.*

class ChallengesSuite extends FunSuite {

  private def response(
      code: Int,
      www: List[String] = Nil,
      proxy: List[String] = Nil
  ) =
    SipResponse(
      code,
      "x",
      SipHeaders(
        callId = callId("c@h"),
        from = nameAddr("<sip:a@h>;tag=1"),
        to = nameAddr("<sip:b@h>"),
        vias =
          NonEmptyList.one(via("SIP/2.0/UDP 127.0.0.1:5060;branch=z9hG4bKx")),
        cseq = cseq(1, SipMethod.Invite),
        wwwAuthenticate = www.map(challenge),
        proxyAuthenticate = proxy.map(challenge)
      )
    )

  private val fresh = """Digest realm="r", nonce="n1", algorithm=MD5"""
  private val stale =
    """Digest realm="r", nonce="n2", algorithm=MD5, stale=true"""

  private def digest(text: String): DigestChallenge =
    AuthChallenge.best(List(challenge(text))).getOrElse(fail(text))

  test(
    "a 401 is the server's challenge and a 407 the proxy's, each from its own header"
  ) {
    assertEquals(
      Challenges.of(response(401, www = List(fresh))).map(_._1),
      Some(AuthKind.Origin)
    )
    assertEquals(
      Challenges.of(response(407, proxy = List(fresh))).map(_._1),
      Some(AuthKind.Proxy)
    )
    assertEquals(
      Challenges.of(response(401, www = List(fresh))).map(_._2.size),
      Some(1)
    )
    // A 401 does not read the proxy's header, nor a 407 the server's.
    assertEquals(
      Challenges.of(response(401, proxy = List(fresh))).map(_._2),
      Some(Nil)
    )
  }

  test("no other response is a challenge") {
    List(200, 403, 404, 486, 500).foreach(code =>
      assertEquals(Challenges.of(response(code)), None, code.toString)
    )
  }

  test("the extractor matches what `of` finds") {
    response(407, proxy = List(fresh)) match {
      case Challenges.Challenged(kind, challenges) =>
        assertEquals((kind, challenges.size), (AuthKind.Proxy, 1))
      case other => fail(s"not a challenge: $other")
    }
    response(200) match {
      case Challenges.Challenged(_, _) => fail("a 200 is not a challenge")
      case _                           => ()
    }
  }

  test(
    "the first challenge is answered, a second only if the nonce went stale, and never a third"
  ) {
    assert(Challenges.mayAnswer(0, digest(fresh)))
    assert(!Challenges.mayAnswer(1, digest(fresh)))
    assert(Challenges.mayAnswer(1, digest(stale)))
    assert(!Challenges.mayAnswer(Challenges.MaxAttempts, digest(stale)))
  }

  test("the challenge to answer is the best of those we can, if we may") {
    assertEquals(
      Challenges.toAnswer(List(challenge(fresh)), 0).map(_.nonce.value),
      Some("n1")
    )
    assertEquals(Challenges.toAnswer(List(challenge(fresh)), 1), None)
    assertEquals(
      Challenges.toAnswer(List(challenge(stale)), 1).map(_.nonce.value),
      Some("n2")
    )
  }

  test("nothing we can answer is nothing to answer") {
    assertEquals(
      Challenges.toAnswer(List(challenge("Basic realm=\"r\"")), 0),
      None
    )
    assertEquals(Challenges.toAnswer(Nil, 0), None)
  }

  test(
    "a request answered once at most takes a challenge only when none was answered, stale or not"
  ) {
    assert(Challenges.once(List(challenge(fresh)), 0).isDefined)
    assert(Challenges.once(List(challenge(stale)), 0).isDefined)
    assertEquals(Challenges.once(List(challenge(stale)), 1), None)
  }
}
