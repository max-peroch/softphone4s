package softphone4s

import softphone4s.model.*

/** Parses protocol literals in tests, failing the test on bad input. */
object SipLiterals {

  private def get[A](e: Either[String, A]): A =
    e.fold(err => throw new AssertionError(err), identity)

  def uri(s: String): SipUri        = get(SipUri.parse(s))
  def nameAddr(s: String): NameAddr = get(NameAddr.parse(s))
  def via(s: String): Via           = get(Via.parse(s))
  def callId(s: String): CallId     = get(CallId(s))
  def tag(s: String): Tag           = get(Tag(s))
  def branch(s: String): Branch     = get(Branch(s))
}
