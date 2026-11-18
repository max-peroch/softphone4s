package softphone4s.model

/** A header field outside the modelled set, carried through unchanged. */
final case class ExtensionHeader(name: FieldName, value: String)
