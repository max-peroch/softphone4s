package softphone4s.model

/** How a request within a dialog is addressed and where it is sent.
  *
  * @param requestUri
  *   the Request-URI to use.
  * @param routes
  *   the Route headers to carry.
  * @param nextHop
  *   the URI whose host and port the request is sent to.
  */
final case class RequestRoute(
    requestUri: SipUri,
    routes: List[NameAddr],
    nextHop: SipUri
)
