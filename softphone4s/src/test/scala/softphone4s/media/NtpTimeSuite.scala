package softphone4s.media

import munit.FunSuite

import scala.concurrent.duration.*

class NtpTimeSuite extends FunSuite {

  test(
    "the Unix epoch is 2 208 988 800 seconds into the NTP era, and no fraction"
  ) {
    assertEquals(NtpTime.fromEpoch(Duration.Zero), 2_208_988_800L << 32)
  }

  test("the year 2000 is where the RFC puts it") {
    // 2000-01-01T00:00:00Z: 946 684 800 s after the Unix epoch.
    assertEquals(
      NtpTime.fromEpoch(946_684_800.seconds) >>> 32,
      3_155_673_600L
    )
  }

  test("half a second is half of the 32-bit fraction") {
    val ntp = NtpTime.fromEpoch(1500.millis)
    assertEquals(ntp >>> 32, 2_208_988_801L)
    assertEquals(ntp & 0xffffffffL, 0x80000000L)
  }

  test(
    "a quarter of a second, and the last nanosecond below a second, do not overflow"
  ) {
    assertEquals(NtpTime.fromEpoch(250.millis) & 0xffffffffL, 0x40000000L)
    val fraction = NtpTime.fromEpoch(999_999_999.nanos) & 0xffffffffL
    assert(fraction > 0xffffff00L && fraction < 0x100000000L, fraction)
  }

  test("the seconds wrap at 32 bits as they do on the wire") {
    // 2036-02-07T06:28:16Z is where the NTP seconds pass 2^32.
    val atWrap = (4_294_967_296L - 2_208_988_800L).seconds
    assertEquals(NtpTime.fromEpoch(atWrap) >>> 32, 0L)
  }
}
