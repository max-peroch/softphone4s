package softphone4s.sdp

import softphone4s.srtp.SrtpError

/** Why an `a=crypto` line cannot be used (RFC 4568 §9.1). */
enum CryptoError {
  case Malformed(text: String)
  case UnknownSuite(name: String)

  /** A key method other than `inline:`, or several keys. */
  case UnsupportedKeyMethod(text: String)

  /** A master key identifier, which puts bytes in every packet that we do not
    * read.
    */
  case UnsupportedMki

  /** A session parameter, such as `KDR=`, which changes how the keys are used.
    */
  case UnsupportedParameter(text: String)
  case BadKey(error: SrtpError)

  def message: String = this match {
    case Malformed(text)            => s"malformed a=crypto: $text"
    case UnknownSuite(name)         => s"unknown crypto suite $name"
    case UnsupportedKeyMethod(text) => s"unsupported key method: $text"
    case UnsupportedMki => "master key identifiers are not supported"
    case UnsupportedParameter(text) => s"unsupported session parameter: $text"
    case BadKey(error)              => s"bad key: ${error.message}"
  }
}
