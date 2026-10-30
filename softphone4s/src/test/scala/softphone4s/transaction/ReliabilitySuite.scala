package softphone4s.transaction

import munit.FunSuite
import softphone4s.model.TransportProtocol

class ReliabilitySuite extends FunSuite {

  test("only UDP leaves retransmission to the transaction") {
    assertEquals(Reliability.of(TransportProtocol.Udp), Reliability.Unreliable)
    List(
      TransportProtocol.Tcp,
      TransportProtocol.Tls,
      TransportProtocol.Sctp,
      TransportProtocol.Ws,
      TransportProtocol.Wss
    )
      .foreach(t =>
        assertEquals(Reliability.of(t), Reliability.Reliable, t.token)
      )
  }
}
