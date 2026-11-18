package softphone4s.model

/** Who challenged us: the user agent server (401) or a proxy (407). */
enum AuthKind(val challengeHeader: FieldName, val responseHeader: FieldName) {
  case Origin
      extends AuthKind(
        FieldName.WwwAuthenticate,
        FieldName.Authorization
      )
  case Proxy
      extends AuthKind(
        FieldName.ProxyAuthenticate,
        FieldName.ProxyAuthorization
      )
}
