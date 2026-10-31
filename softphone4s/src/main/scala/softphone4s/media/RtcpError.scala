package softphone4s.media

/** Why bytes could not be read as RTCP. */
enum RtcpError {
  case Truncated(needed: Int, available: Int)
  case BadVersion(version: Int)
  case BadLength(packetType: Int, words: Int)
  case BadPadding(packetType: Int)

  def message: String = this match {
    case Truncated(needed, available) =>
      s"packet needs $needed bytes, $available are left"
    case BadVersion(version)         => s"version $version is not 2"
    case BadLength(packetType, size) =>
      s"packet type $packetType does not fit in $size words"
    case BadPadding(packetType) => s"bad padding in packet type $packetType"
  }
}
