package softphone4s.transaction

/** The timers of RFC 3261 §17, named by their letters.
  *
  * Client: A, E retransmit the request; B, F time out; D, K absorb response
  * retransmits. Server: G retransmits the final response; H waits for the ACK;
  * I absorbs ACK retransmits; J absorbs request retransmits.
  */
enum TransactionTimer {
  case A, B, D, E, F, G, H, I, J, K
}
