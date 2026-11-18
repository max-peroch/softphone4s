package softphone4s.model

/** A single DTMF tone, tagged with its RFC 4733 telephone-event code. */
enum DtmfDigit(val code: Int, val symbol: Char) {
  case D0   extends DtmfDigit(0, '0')
  case D1   extends DtmfDigit(1, '1')
  case D2   extends DtmfDigit(2, '2')
  case D3   extends DtmfDigit(3, '3')
  case D4   extends DtmfDigit(4, '4')
  case D5   extends DtmfDigit(5, '5')
  case D6   extends DtmfDigit(6, '6')
  case D7   extends DtmfDigit(7, '7')
  case D8   extends DtmfDigit(8, '8')
  case D9   extends DtmfDigit(9, '9')
  case Star extends DtmfDigit(10, '*')
  case Hash extends DtmfDigit(11, '#')
  case A    extends DtmfDigit(12, 'A')
  case B    extends DtmfDigit(13, 'B')
  case C    extends DtmfDigit(14, 'C')
  case D    extends DtmfDigit(15, 'D')
}

object DtmfDigit {

  /** Looks up the digit for an RFC 4733 event code, e.g. `10` → `Star`. */
  def fromCode(code: Int): Option[DtmfDigit] = values.find(_.code == code)

  /** The digit a character stands for, as SIP INFO writes it: `5`, `*`, `#`,
    * `A` to `D` (in either case).
    */
  def fromSymbol(symbol: Char): Option[DtmfDigit] =
    values.find(_.symbol == symbol.toUpper)
}
