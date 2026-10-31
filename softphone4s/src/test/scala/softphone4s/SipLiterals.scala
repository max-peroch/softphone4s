package softphone4s

import softphone4s.model.*

/** Parses protocol literals in tests, failing the test on bad input. */
object SipLiterals extends munit.Assertions {

  private def get[A](e: Either[String, A]): A =
    e.fold(err => fail(err), identity)

  def uri(s: String): SipUri                = get(SipUri.parse(s))
  def nameAddr(s: String): NameAddr         = get(NameAddr.parse(s))
  def via(s: String): Via                   = get(Via.parse(s))
  def callId(s: String): CallId             = get(CallId(s))
  def tag(s: String): Tag                   = get(Tag(s))
  def branch(s: String): Branch             = get(Branch(s))
  def fieldName(s: String): FieldName       = get(FieldName(s))
  def cseq(n: Int, method: SipMethod): CSeq =
    CSeq(get(CSeqNumber(n)), method)
  def cseqNumber(n: Int): CSeqNumber      = get(CSeqNumber(n))
  def challenge(s: String): AuthChallenge = AuthChallenge.parse(s)
  def pt(n: Int): PayloadType             = get(PayloadType(n))
}
