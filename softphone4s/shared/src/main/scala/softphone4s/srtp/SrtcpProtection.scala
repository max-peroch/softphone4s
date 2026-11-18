package softphone4s.srtp

/** Whether an SRTCP packet is encrypted: the E flag of its index word. */
private[srtp] enum SrtcpProtection {
  case Encrypted
  case Plain
}
