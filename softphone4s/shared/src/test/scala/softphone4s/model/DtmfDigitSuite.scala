package softphone4s.model

import munit.FunSuite

class DtmfDigitSuite extends FunSuite {

  test("fromCode resolves digits") {
    assertEquals(DtmfDigit.fromCode(0), Some(DtmfDigit.D0))
    assertEquals(DtmfDigit.fromCode(9), Some(DtmfDigit.D9))
  }

  test("fromCode resolves star, hash, and A-D") {
    assertEquals(DtmfDigit.fromCode(10), Some(DtmfDigit.Star))
    assertEquals(DtmfDigit.fromCode(11), Some(DtmfDigit.Hash))
    assertEquals(DtmfDigit.fromCode(12), Some(DtmfDigit.A))
    assertEquals(DtmfDigit.fromCode(15), Some(DtmfDigit.D))
  }

  test("fromCode rejects codes outside 0-15") {
    assertEquals(DtmfDigit.fromCode(-1), None)
    assertEquals(DtmfDigit.fromCode(16), None)
  }

  test("code round-trips through fromCode") {
    DtmfDigit.values.foreach { d =>
      assertEquals(DtmfDigit.fromCode(d.code), Some(d))
    }
  }
}
