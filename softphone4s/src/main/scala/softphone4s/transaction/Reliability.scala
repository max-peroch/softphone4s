package softphone4s.transaction

/** Whether the transport retransmits for us. Over a reliable one (TCP, TLS)
  * transactions skip their retransmit timers and wait times (RFC 3261
  * §17.1.1.2).
  */
enum Reliability {
  case Reliable, Unreliable
}
