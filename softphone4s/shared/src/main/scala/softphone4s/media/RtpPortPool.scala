package softphone4s.media

import com.comcast.ip4s.Port

/** Which pairs of the range are in use, and where the last one was taken. The
  * next is taken after that, going round, so that a pair that has been given
  * back is not given out again until all the others have been tried: a late
  * packet for a call that is over finds no new call there.
  *
  * It is a pure value; [[RtpPorts]] is the effectful one that keeps it.
  */
final case class RtpPortPool private (
    range: RtpPortRange,
    inUse: Set[Int],
    cursor: Int
) {

  /** The free pairs, in the order they are to be tried, at most `limit`. */
  def candidates(limit: Int): List[Int] =
    Iterator
      .range(1, range.pairs + 1)
      .map(step => (cursor + step) % range.pairs)
      .filterNot(inUse)
      .take(limit)
      .toList

  /** The pool with pair `index` taken, if it was free. */
  def reserve(index: Int): Option[RtpPortPool] =
    Option.unless(inUse.contains(index))(
      copy(inUse = inUse + index, cursor = index)
    )

  /** The pool with pair `index` given up as unusable: free again, but not to be
    * tried before the others.
    */
  def skip(index: Int): RtpPortPool = copy(inUse = inUse - index)

  /** The pool with the pair whose RTP port is `port` given back. */
  def release(port: Port): RtpPortPool =
    range.indexOf(port).fold(this)(index => copy(inUse = inUse - index))

  def isEmpty: Boolean = inUse.isEmpty
}

object RtpPortPool {

  /** A pool with every pair free; the first taken is the first of the range. */
  def apply(range: RtpPortRange): RtpPortPool =
    new RtpPortPool(range, Set.empty, range.pairs - 1)
}
