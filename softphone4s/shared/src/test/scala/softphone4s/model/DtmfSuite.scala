package softphone4s.model

import munit.FunSuite

import scala.concurrent.duration.*

class DtmfSuite extends FunSuite {

  test("every digit has a symbol and an event number that find it again") {
    DtmfDigit.values.foreach { digit =>
      assertEquals(DtmfDigit.fromSymbol(digit.symbol), Some(digit))
      assertEquals(DtmfDigit.fromCode(digit.code), Some(digit))
    }
    assertEquals(DtmfDigit.values.map(_.symbol).distinct.length, 16)
    assertEquals(DtmfDigit.values.map(_.code).toList, (0 to 15).toList)
  }

  test("symbols are the keys of a phone, A to D in either case") {
    assertEquals(
      DtmfDigit.values.map(_.symbol).mkString,
      "0123456789*#ABCD"
    )
    assertEquals(DtmfDigit.fromSymbol('b'), Some(DtmfDigit.B))
    assertEquals(DtmfDigit.fromSymbol('x'), None)
  }

  test("a telephone event is four bytes that decode to what was encoded") {
    List(
      SoftphoneEvent(5, endBit = false, volume = 10, duration = 160),
      SoftphoneEvent(15, endBit = true, volume = 63, duration = 65535),
      SoftphoneEvent(0, endBit = true, volume = 0, duration = 0)
    ).foreach { event =>
      val bytes = SoftphoneEvent.encode(event)
      assertEquals(bytes.size, 4)
      assertEquals(SoftphoneEvent.decode(bytes), Some(event))
    }
  }

  test("the end bit is the top bit of the second byte, the volume the rest") {
    val bytes = SoftphoneEvent.encode(SoftphoneEvent(1, endBit = true, 10, 800))
    assertEquals(bytes.toList.map(_ & 0xff), List(1, 0x8a, 0x03, 0x20))
  }

  test("a digit lasts from 20 ms to 8 s") {
    List(20.millis, 160.millis, 8.seconds).foreach(d =>
      assertEquals(Dtmf.checkedDuration(d), Right(d))
    )
    List(0.millis, 19.millis, 8001.millis).foreach(d =>
      assertEquals(
        Dtmf.checkedDuration(d),
        Left(SoftphoneError.InvalidDtmfDuration(d.toMillis))
      )
    )
  }
}
