package softphone4s.model

import munit.FunSuite

class HeaderTypesSuite extends FunSuite {

  test(
    "CSeqNumber accepts 0 and up, rejects negatives, and saturates on next"
  ) {
    assert(CSeqNumber(0).isRight)
    assert(CSeqNumber(-1).isLeft)
    assertEquals(CSeqNumber.One.next.value, 2)
    assertEquals(
      CSeqNumber(Int.MaxValue).map(_.next.value),
      Right(Int.MaxValue)
    )
  }

  test("MaxForwards is 0 to 255 and decrements down to None") {
    assert(MaxForwards(256).isLeft)
    assert(MaxForwards(-1).isLeft)
    assertEquals(MaxForwards.Default.value, 70)
    assertEquals(MaxForwards(1).map(_.decrement.map(_.value)), Right(Some(0)))
    assertEquals(MaxForwards(0).map(_.decrement), Right(None))
  }

  test("NonceCount renders as eight hex digits and counts up from one") {
    assertEquals(NonceCount.One.render, "00000001")
    assertEquals(NonceCount(255).map(_.render), Right("000000ff"))
    assert(NonceCount(0).isLeft)
  }

  test("MediaType lower-cases type and subtype and keeps parameters") {
    MediaType.parse("Application/SDP; charset=utf-8") match {
      case Right(mt) =>
        assertEquals(mt.mainType, "application")
        assertEquals(mt.subType, "sdp")
        assertEquals(mt.render, "application/sdp;charset=utf-8")
      case Left(err) => fail(err)
    }
    assertEquals(MediaType.parse("application/sdp"), Right(MediaType.Sdp))
  }

  test("MediaType rejects malformed values") {
    List(
      "",
      "application",
      "/sdp",
      "application/",
      "app lication/sdp",
      "a/b;x=1;x=2"
    )
      .foreach(s =>
        assert(MediaType.parse(s).isLeft, s"expected Left for '$s'")
      )
  }

  test("AuthKind names the right headers") {
    assertEquals(AuthKind.Origin.challengeHeader.value, "WWW-Authenticate")
    assertEquals(AuthKind.Origin.responseHeader.value, "Authorization")
    assertEquals(AuthKind.Proxy.challengeHeader.value, "Proxy-Authenticate")
    assertEquals(AuthKind.Proxy.responseHeader.value, "Proxy-Authorization")
  }

  test("Expires is a number of seconds, read as digits only") {
    assertEquals(Expires.parse("3600").map(_.value), Right(3600))
    assertEquals(Expires.parse(" 60 ").map(_.value), Right(60))
    List("", "-1", "+5", "1.5", "abc", "99999999999", "12 34").foreach(s =>
      assert(Expires.parse(s).isLeft, s"expected Left for '$s'")
    )
    assert(Expires(-1).isLeft)
  }

  test("Expires converts to and from durations") {
    import scala.concurrent.duration.*
    assertEquals(Expires.of(90.seconds).value, 90)
    assertEquals(Expires.of(1500.millis).value, 1)
    assertEquals(Expires.of((-5).seconds), Expires.Zero)
    assertEquals(Expires.parse("120").map(_.duration), Right(120.seconds))
    assert(Expires.Zero.isZero)
  }
}
