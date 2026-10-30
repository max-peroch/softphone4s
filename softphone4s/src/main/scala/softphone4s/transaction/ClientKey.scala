package softphone4s.transaction

import softphone4s.model.*

/** Identifies a client transaction: the branch of the request's topmost Via and
  * its method (RFC 3261 §17.1.3). A response is matched by its topmost Via
  * branch and the method in its CSeq.
  */
final case class ClientKey(branch: Branch, method: SipMethod)

object ClientKey {

  def forRequest(request: SipRequest): Either[TransactionKeyError, ClientKey] =
    branchOf(request.headers.topVia).map(ClientKey(_, request.method))

  def forResponse(
      response: SipResponse
  ): Either[TransactionKeyError, ClientKey] =
    branchOf(response.headers.topVia).map(
      ClientKey(_, response.headers.cseq.method)
    )

  private[transaction] def branchOf(
      via: Via
  ): Either[TransactionKeyError, Branch] =
    via.branch match {
      case None                    => Left(TransactionKeyError.MissingBranch)
      case Some(b) if !b.isRfc3261 => Left(TransactionKeyError.Rfc2543Branch(b))
      case Some(b)                 => Right(b)
    }
}
