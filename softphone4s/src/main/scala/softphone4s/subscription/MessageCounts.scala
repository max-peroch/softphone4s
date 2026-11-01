package softphone4s.subscription

/** How many messages of one class a mailbox holds: new and old, and of those
  * the urgent ones.
  */
final case class MessageCounts private (
    newMessages: Int,
    oldMessages: Int,
    urgentNew: Int,
    urgentOld: Int
)

object MessageCounts {

  /** Counts that are not negative, the urgent ones no more than the others. */
  def of(
      newMessages: Int,
      oldMessages: Int,
      urgentNew: Int = 0,
      urgentOld: Int = 0
  ): Either[String, MessageCounts] =
    Either.cond(
      List(newMessages, oldMessages, urgentNew, urgentOld).forall(_ >= 0) &&
        urgentNew <= newMessages && urgentOld <= oldMessages,
      new MessageCounts(newMessages, oldMessages, urgentNew, urgentOld),
      s"impossible counts $newMessages/$oldMessages ($urgentNew/$urgentOld)"
    )
}
