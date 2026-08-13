package softphone4s.model

/** Failures raised by `Softphone`/`Call` operations. See the README's Errors
  * table for what triggers each case.
  */
sealed abstract class SoftphoneError(message: String)
    extends Exception(message)
    with scala.util.control.NoStackTrace

object SoftphoneError {
  final case class InvalidHostname(host: String)
      extends SoftphoneError(s"Invalid hostname: '$host'")
  final case class HostResolutionFailed(host: String)
      extends SoftphoneError(s"Cannot resolve SIP server '$host'")
  final case class CallRejected(code: Int, reason: String)
      extends SoftphoneError(s"Call rejected: $code $reason")
  case object CallTimedOut extends SoftphoneError("INVITE timed out")
  case object CallCancelled
      extends SoftphoneError("Call cancelled before answer")
  case object RemoteHangup extends SoftphoneError("Remote party ended the call")
  final case class RtpPortExhausted(port: Int)
      extends SoftphoneError(
        s"Computed RTP port $port is out of range (0-65535)"
      )
}
