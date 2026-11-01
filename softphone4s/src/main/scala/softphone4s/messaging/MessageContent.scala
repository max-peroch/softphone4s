package softphone4s.messaging

import softphone4s.model.MediaType

/** What an instant message (RFC 3428) says: text, and the type it is in. Bodies
  * go over the wire as UTF-8, so what is sent is text; `text/plain` is the type
  * every phone takes.
  */
final case class MessageContent(contentType: MediaType, text: String)

object MessageContent {

  /** `text` as `text/plain`. */
  def plain(text: String): MessageContent =
    MessageContent(MediaType.Plain, text)
}
