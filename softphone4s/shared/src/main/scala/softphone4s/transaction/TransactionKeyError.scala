package softphone4s.transaction

import softphone4s.model.Branch

/** Why a message cannot be matched to a transaction. */
enum TransactionKeyError {

  /** The topmost Via has no `branch` parameter. */
  case MissingBranch

  /** The branch lacks the RFC 3261 magic cookie, so the message comes from an
    * RFC 2543 element, whose matching rules (§17.2.3) are not supported.
    */
  case Rfc2543Branch(branch: Branch)

  def message: String = this match {
    case MissingBranch    => "the topmost Via has no branch parameter"
    case Rfc2543Branch(b) => s"branch '${b.value}' is not an RFC 3261 branch"
  }
}
