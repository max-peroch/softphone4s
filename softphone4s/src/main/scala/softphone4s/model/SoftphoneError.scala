package softphone4s.model

/** Failures raised by `Softphone`/`Call` operations. See the README's Errors
  * table for what triggers each case.
  */
sealed abstract class SoftphoneError(message: String)
    extends Exception(message)
    with scala.util.control.NoStackTrace

object SoftphoneError {
  final case class InvalidSipUri(input: String, reason: String)
      extends SoftphoneError(s"Invalid SIP URI '$input': $reason")
  final case class InvalidHeader(name: String, reason: String)
      extends SoftphoneError(s"Invalid header '$name': $reason")
  final case class HostResolutionFailed(host: String)
      extends SoftphoneError(s"Cannot resolve SIP server '$host'")
  final case class CallRejected(code: Int, reason: String)
      extends SoftphoneError(s"Call rejected: $code $reason")
  case object CallTimedOut extends SoftphoneError("INVITE timed out")
  case object CallCancelled
      extends SoftphoneError("Call cancelled before answer")
  case object RemoteHangup extends SoftphoneError("Remote party ended the call")
  case object TransportFailed
      extends SoftphoneError(
        "The SIP transport failed while signalling the call"
      )
  final case class TransportUnavailable(transport: String)
      extends SoftphoneError(s"No $transport transport is available")
  final case class TlsConfigurationInvalid(reason: String)
      extends SoftphoneError(s"Cannot set up TLS: $reason")
  final case class UnknownAccount(account: String)
      extends SoftphoneError(s"No such account: $account")
  final case class DuplicateAccount(account: String)
      extends SoftphoneError(s"Account configured twice: $account")
  final case class RtpPortExhausted(port: Int)
      extends SoftphoneError(
        s"Computed RTP port $port is out of range (0-65535)"
      )
}
