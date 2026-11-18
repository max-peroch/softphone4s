package softphone4s.model

/** Whether a Digest challenge says our nonce merely expired (`stale=true`, RFC
  * 2617 §3.2.1), so the same credentials are worth sending again with the new
  * nonce.
  */
enum Staleness {
  case Fresh, Stale
}
