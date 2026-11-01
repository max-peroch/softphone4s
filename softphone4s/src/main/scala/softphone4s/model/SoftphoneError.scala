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
  final case class ConnectTimedOut(address: String)
      extends SoftphoneError(s"Timed out connecting to $address")
  final case class TransportUnavailable(transport: String)
      extends SoftphoneError(s"No $transport transport is available")
  case object NoCodecsConfigured
      extends SoftphoneError("No codec is configured")
  case object CallNotEstablished
      extends SoftphoneError("The call is not established")
  final case class InvalidDtmfDuration(milliseconds: Long)
      extends SoftphoneError(
        s"A digit lasts from 20 ms to 8 s, not $milliseconds ms"
      )
  final case class UnknownCodec(token: String)
      extends SoftphoneError(s"No codec called $token")
  final case class DuplicateCodec(token: String)
      extends SoftphoneError(s"Two codecs are called $token")
  case object MixedCodecRates
      extends SoftphoneError(
        "The codecs work at different sample rates, which cannot be played on one audio format"
      )
  final case class AudioFormatMismatch(codecRate: Int, deviceRate: Int)
      extends SoftphoneError(
        s"The codecs work at $codecRate Hz but the audio format is $deviceRate Hz"
      )
  final case class CodecUnavailable(codec: String)
      extends SoftphoneError(s"No implementation of the codec $codec")
  final case class TlsConfigurationInvalid(reason: String)
      extends SoftphoneError(s"Cannot set up TLS: $reason")
  final case class UnknownAccount(account: String)
      extends SoftphoneError(s"No such account: $account")
  final case class DuplicateAccount(account: String)
      extends SoftphoneError(s"Account configured twice: $account")
  final case class RtpPortsExhausted(first: Int, last: Int)
      extends SoftphoneError(
        s"No free pair of RTP ports from $first to $last"
      )
  final case class RtpPortsInvalid(reason: String)
      extends SoftphoneError(s"Cannot use these RTP ports: $reason")
}
