package softphone4s.media

import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

/** How big a jitter buffer is, in frames of 20 ms.
  *
  * @param depth
  *   how many frames are held before playing begins, and the cushion that
  *   absorbs jitter and reordering; the delay it adds is `depth` frames. It is
  *   the least the buffer will use.
  * @param capacity
  *   the most frames ever held, which bounds the memory and the delay: when
  *   more arrive, the oldest are dropped.
  * @param maxDepth
  *   the most the cushion grows to when the network gives the buffer reason to
  *   (see [[JitterBuffer]]); equal to `depth` for a buffer that does not adapt.
  */
final case class JitterConfig private (
    depth: Int,
    capacity: Int,
    maxDepth: Int
)

object JitterConfig {

  /** The keys of the configuration, each optional: the cushion in frames, the
    * room in frames, and how far the cushion may grow, which is twice the
    * cushion, or as far as there is room, if not said. `max-depth = depth` is a
    * buffer that does not adapt.
    */
  private final case class Raw(
      depth: Int = 3,
      capacity: Int = 12,
      maxDepth: Option[Int] = None
  ) derives ConfigReader

  given ConfigReader[JitterConfig] = ConfigReader[Raw].emap(raw =>
    adaptive(
      raw.depth,
      raw.maxDepth.getOrElse(math.min(raw.depth * 2, raw.capacity)),
      raw.capacity
    ).left.map(
      CannotConvert(
        s"${raw.depth}, ${raw.capacity}, ${raw.maxDepth}",
        "JitterConfig",
        _
      )
    )
  )

  /** Three frames, 60 ms, of cushion, growing to six, 120 ms, and room for
    * twelve, 240 ms.
    */
  val Default: JitterConfig = new JitterConfig(3, 12, 6)

  /** A buffer of `depth` frames that stays that size. */
  def apply(depth: Int, capacity: Int): Either[String, JitterConfig] =
    adaptive(depth, depth, capacity)

  /** A buffer that starts at `depth` frames and grows to at most `maxDepth`. */
  def adaptive(
      depth: Int,
      maxDepth: Int,
      capacity: Int
  ): Either[String, JitterConfig] =
    for {
      _ <- Either.cond(depth >= 1, (), s"the depth is at least 1, not $depth")
      _ <- Either.cond(
        maxDepth >= depth,
        (),
        s"the maximum depth, $maxDepth, is at least the depth, $depth"
      )
      _ <- Either.cond(
        capacity >= maxDepth,
        (),
        s"the capacity, $capacity, is at least the maximum depth, $maxDepth"
      )
    } yield new JitterConfig(depth, capacity, maxDepth)
}
