package softphone4s.sdp

/** The first field of an `m=` line. */
enum MediaKind(val token: String) {
  case Audio               extends MediaKind("audio")
  case Video               extends MediaKind("video")
  case Text                extends MediaKind("text")
  case Application         extends MediaKind("application")
  case Message             extends MediaKind("message")
  case Other(name: String) extends MediaKind(name)
}

object MediaKind {
  def fromToken(token: String): MediaKind =
    List(Audio, Video, Text, Application, Message)
      .find(_.token == token)
      .getOrElse(Other(token))
}
