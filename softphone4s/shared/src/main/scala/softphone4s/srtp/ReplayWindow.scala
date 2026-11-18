package softphone4s.srtp

/** The record against replay of RFC 3711 §3.3.2: the highest index opened, and
  * which of the 64 indexes up to it have been. A packet is counted once it has
  * been authenticated, never before, so that one forged cannot move the window.
  */
private[srtp] final case class ReplayWindow(highest: Option[Long], seen: Long) {

  /** Whether the packet of this index may be opened: not one that is in the
    * window already, and not one behind it.
    */
  def admits(index: Long): Either[SrtpError, Unit] =
    highest match {
      case Some(top) if index <= top =>
        val behind = top - index
        if behind >= ReplayWindow.Size then Left(SrtpError.TooOld)
        else if ((seen >>> behind) & 1L) != 0 then Left(SrtpError.Replayed)
        else Right(())
      case _ => Right(())
    }

  /** The window once the packet of this index has been opened. */
  def counted(index: Long): ReplayWindow =
    highest match {
      case Some(top) if index <= top =>
        ReplayWindow(highest, seen | (1L << (top - index)))
      case Some(top) =>
        val shift = index - top
        ReplayWindow(
          Some(index),
          if shift >= ReplayWindow.Size then 1L else (seen << shift) | 1L
        )
      case None => ReplayWindow(Some(index), 1L)
    }
}

private[srtp] object ReplayWindow {

  /** How many indexes the window covers. */
  val Size: Int = 64

  val Empty: ReplayWindow = ReplayWindow(None, 0L)
}
