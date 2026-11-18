package softphone4s.model

/** Digest quality of protection (RFC 2617 §3.2.1). */
enum Qop(val token: String) {
  case Auth    extends Qop("auth")
  case AuthInt extends Qop("auth-int")
}

object Qop {
  def fromToken(s: String): Option[Qop] =
    values.find(_.token.equalsIgnoreCase(s.trim))
}
