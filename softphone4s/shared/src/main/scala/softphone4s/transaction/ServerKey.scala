package softphone4s.transaction

import com.comcast.ip4s.{Host, Port}
import softphone4s.model.*

/** Identifies a server transaction: the branch and sent-by of the request's
  * topmost Via, and its method, where an ACK counts as INVITE so that it finds
  * the transaction of the failure it acknowledges (RFC 3261 §17.2.3). CANCEL is
  * its own transaction.
  */
final case class ServerKey(
    branch: Branch,
    host: Host,
    port: Option[Port],
    method: SipMethod
)

object ServerKey {

  def forRequest(
      request: SipRequest
  ): Either[TransactionKeyError, ServerKey] = {
    val via = request.headers.topVia
    ClientKey.branchOf(via).map { branch =>
      ServerKey(
        branch,
        via.host,
        via.port,
        if request.method == SipMethod.Ack then SipMethod.Invite
        else request.method
      )
    }
  }
}
