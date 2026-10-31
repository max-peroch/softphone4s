package softphone4s.opus

/** Whether each packet also carries a rougher copy of the one before, to cover
  * a loss (RFC 6716 §2.1.7). It costs bit rate and helps when packets are lost.
  */
enum InbandFec(val flag: Int) {
  case Enabled  extends InbandFec(1)
  case Disabled extends InbandFec(0)
}
