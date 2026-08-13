package softphone4s.model

/** A single DTMF tone, tagged with its RFC 4733 telephone-event code. */
enum DtmfDigit(val code: Int) {
  case D0   extends DtmfDigit(0)
  case D1   extends DtmfDigit(1)
  case D2   extends DtmfDigit(2)
  case D3   extends DtmfDigit(3)
  case D4   extends DtmfDigit(4)
  case D5   extends DtmfDigit(5)
  case D6   extends DtmfDigit(6)
  case D7   extends DtmfDigit(7)
  case D8   extends DtmfDigit(8)
  case D9   extends DtmfDigit(9)
  case Star extends DtmfDigit(10)
  case Hash extends DtmfDigit(11)
  case A    extends DtmfDigit(12)
  case B    extends DtmfDigit(13)
  case C    extends DtmfDigit(14)
  case D    extends DtmfDigit(15)
}

object DtmfDigit {

  /** Looks up the digit for an RFC 4733 event code, e.g. `10` → `Star`. */
  def fromCode(code: Int): Option[DtmfDigit] = values.find(_.code == code)
}
