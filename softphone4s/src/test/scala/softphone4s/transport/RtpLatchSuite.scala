package softphone4s.transport

import com.comcast.ip4s.*
import munit.FunSuite

class RtpLatchSuite extends FunSuite {

  private val signalled = SocketAddress(ip"198.51.100.1", port"4000")
  private val actual    = SocketAddress(ip"203.0.113.5", port"61000")

  /** Feeds packets in turn; the target after each one. */
  private def run(
      packets: List[(SocketAddress[IpAddress], Long)],
      from: Option[SocketAddress[IpAddress]] = Some(signalled)
  ): List[Option[SocketAddress[IpAddress]]] =
    packets
      .foldLeft(
        (RtpLatch.empty, from, List.empty[Option[SocketAddress[IpAddress]]])
      ) { case ((latch, target, seen), (source, ssrc)) =>
        val (next, moved) = RtpLatch.hear(latch, target, source, ssrc)
        val now           = moved.orElse(target)
        (next, now, seen :+ now)
      }
      ._3

  test("packets from the target change nothing") {
    assertEquals(
      run(List.fill(20)((signalled, 1L))).distinct,
      List(Some(signalled))
    )
  }

  test("a run of packets from another source moves the target there") {
    val seen = run(List.fill(RtpLatch.Threshold)((actual, 1L)))
    assertEquals(seen.init.distinct, List(Some(signalled)))
    assertEquals(seen.last, Some(actual))
  }

  test("a run that is broken by the real source starts again") {
    val packets =
      List.fill(RtpLatch.Threshold - 1)((actual, 1L)) :::
        List((signalled, 1L)) :::
        List.fill(RtpLatch.Threshold - 1)((actual, 1L))
    assertEquals(run(packets).distinct, List(Some(signalled)))
  }

  test("packets of another stream from the same source do not add up") {
    val packets = (1 to RtpLatch.Threshold).toList.map(n => (actual, n.toLong))
    assertEquals(run(packets).distinct, List(Some(signalled)))
  }

  test("a source that alternates with another never gets there") {
    val other   = SocketAddress(ip"203.0.113.6", port"61001")
    val packets =
      List.fill(10)(List((actual, 1L), (other, 1L))).flatten
    assertEquals(run(packets).distinct, List(Some(signalled)))
  }

  test("nothing moves while there is no target, as when on hold") {
    assertEquals(
      run(List.fill(20)((actual, 1L)), from = None).distinct,
      List(None)
    )
  }
}
