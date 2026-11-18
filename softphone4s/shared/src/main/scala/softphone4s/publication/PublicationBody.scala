package softphone4s.publication

import softphone4s.model.MediaType

/** The state a PUBLISH carries, of the type its event package defines. */
final case class PublicationBody(contentType: MediaType, text: String)
