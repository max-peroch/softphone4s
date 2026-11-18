package softphone4s.dns

import scala.annotation.tailrec

/** The order to try the targets of an SRV set in (RFC 2782): lowest priority
  * first, and within a priority a weighted random choice, so that load spreads
  * in proportion to the weights.
  */
object SrvOrdering {

  /** `records` in the order to try them. `draws` are random numbers in [0, 1),
    * one used per record chosen; if there are too few the rest are taken as 0,
    * which picks them in listed order. The same draws give the same order.
    */
  def order(records: List[SrvRecord], draws: List[Double]): List[SrvRecord] = {
    val byPriority   = records.groupBy(_.priority).toList.sortBy(_._1)
    val (ordered, _) =
      byPriority.foldLeft((List.empty[SrvRecord], draws)) {
        case ((acc, remaining), (_, group)) =>
          val (chosen, left) = weighted(group, remaining)
          (acc ++ chosen, left)
      }
    ordered
  }

  /** One priority's records, each pick being proportional to weight. Records of
    * weight 0 go first in the running list, so that they stay possible though
    * unlikely when others have weight.
    */
  private def weighted(
      group: List[SrvRecord],
      draws: List[Double]
  ): (List[SrvRecord], List[Double]) = {
    @tailrec
    def go(
        pool: List[SrvRecord],
        draws: List[Double],
        acc: List[SrvRecord]
    ): (List[SrvRecord], List[Double]) =
      (pool.filter(_.weight == 0) ++ pool.filter(_.weight > 0)) match {
        case Nil                         => (acc.reverse, draws)
        case listed @ (firstListed :: _) =>
          val (draw, rest) = draws match {
            case d :: tail => (d.max(0.0).min(0.999999999), tail)
            case Nil       => (0.0, Nil)
          }
          val total  = listed.map(_.weight.toLong).sum
          val point  = (draw * (total + 1)).toLong
          val picked = listed
            .scanLeft((Option.empty[SrvRecord], 0L)) { case ((_, sum), r) =>
              (Some(r), sum + r.weight)
            }
            .collectFirst { case (Some(r), sum) if sum >= point => r }
            .getOrElse(firstListed)
          go(pool.diff(List(picked)), rest, picked :: acc)
      }
    go(group, draws, Nil)
  }
}
