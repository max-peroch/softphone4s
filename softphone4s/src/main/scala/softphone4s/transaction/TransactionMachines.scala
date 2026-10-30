package softphone4s.transaction

import softphone4s.model.SipRequest

/** A client transaction of either kind, so the manager can hold them in one
  * table.
  */
private[transaction] enum ClientMachine {
  case Invite(state: InviteClientState)
  case NonInvite(state: NonInviteClientState)

  def isTerminated: Boolean = this match {
    case Invite(state)    => state == InviteClientState.Terminated
    case NonInvite(state) => state == NonInviteClientState.Terminated
  }

  def step(event: ClientEvent): (ClientMachine, List[ClientAction]) =
    this match {
      case Invite(state) =>
        val (next, actions) =
          InviteClientTransaction.transition(event).run(state).value
        (Invite(next), actions)
      case NonInvite(state) =>
        val (next, actions) =
          NonInviteClientTransaction.transition(event).run(state).value
        (NonInvite(next), actions)
    }
}

private[transaction] object ClientMachine {

  def start(
      request: SipRequest,
      reliability: Reliability,
      config: TransactionConfig
  ): (ClientMachine, List[ClientAction]) =
    if request.method == softphone4s.model.SipMethod.Invite then {
      val (state, actions) =
        InviteClientTransaction.start(request, reliability, config)
      (Invite(state), actions)
    } else {
      val (state, actions) =
        NonInviteClientTransaction.start(request, reliability, config)
      (NonInvite(state), actions)
    }
}

/** A server transaction of either kind. */
private[transaction] enum ServerMachine {
  case Invite(state: InviteServerState)
  case NonInvite(state: NonInviteServerState)

  def isTerminated: Boolean = this match {
    case Invite(state)    => state == InviteServerState.Terminated
    case NonInvite(state) => state == NonInviteServerState.Terminated
  }

  def step(event: ServerEvent): (ServerMachine, List[ServerAction]) =
    this match {
      case Invite(state) =>
        val (next, actions) =
          InviteServerTransaction.transition(event).run(state).value
        (Invite(next), actions)
      case NonInvite(state) =>
        val (next, actions) =
          NonInviteServerTransaction.transition(event).run(state).value
        (NonInvite(next), actions)
    }
}

private[transaction] object ServerMachine {

  def start(
      request: SipRequest,
      reliability: Reliability,
      config: TransactionConfig
  ): (ServerMachine, List[ServerAction]) =
    if request.method == softphone4s.model.SipMethod.Invite then {
      val (state, actions) =
        InviteServerTransaction.start(request, reliability, config)
      (Invite(state), actions)
    } else {
      val (state, actions) =
        NonInviteServerTransaction.start(request, reliability, config)
      (NonInvite(state), actions)
    }
}
