package softphone4s.transaction

import softphone4s.model.TransportProtocol

/** Whether the transport retransmits for us. Over a reliable one (TCP, TLS)
  * transactions skip their retransmit timers and wait times (RFC 3261
  * §17.1.1.2).
  */
enum Reliability {
  case Reliable, Unreliable
}

object Reliability {

  /** Only UDP leaves retransmission to the transaction. */
  def of(transport: TransportProtocol): Reliability = transport match {
    case TransportProtocol.Udp => Unreliable
    case TransportProtocol.Tcp | TransportProtocol.Tls |
        TransportProtocol.Sctp | TransportProtocol.Ws | TransportProtocol.Wss =>
      Reliable
  }
}
