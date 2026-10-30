package softphone4s.model

import com.comcast.ip4s.Port

/** The `rport` Via parameter (RFC 3581): a request asks for it, and the server
  * fills in the port it saw the request come from.
  */
enum RPort {
  case Requested
  case Assigned(port: Port)
}
