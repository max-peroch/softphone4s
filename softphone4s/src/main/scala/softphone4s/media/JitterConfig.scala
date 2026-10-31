package softphone4s.media

import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

/** How big a jitter buffer is, in frames of 20 ms.
  *
  * @param depth
  *   how many frames are held before playing begins, and the cushion that
  *   absorbs jitter and reordering; the delay it adds is `depth` frames.
  * @param capacity
  *   the most frames ever held, which bounds the memory and the delay: when
  *   more arrive, the oldest are dropped.
  */
final case class JitterConfig private (depth: Int, capacity: Int)

object JitterConfig {

  /** The keys of the configuration, each optional: the cushion in frames and
    * the room in frames.
    */
  private final case class Raw(depth: Int = 3, capacity: Int = 12)
      derives ConfigReader

  given ConfigReader[JitterConfig] = ConfigReader[Raw].emap(raw =>
    apply(raw.depth, raw.capacity).left
      .map(CannotConvert(s"${raw.depth}, ${raw.capacity}", "JitterConfig", _))
  )

  /** Three frames, 60 ms, of cushion, and room for twelve, 240 ms. */
  val Default: JitterConfig = new JitterConfig(3, 12)

  def apply(depth: Int, capacity: Int): Either[String, JitterConfig] =
    for {
      _ <- Either.cond(depth >= 1, (), s"the depth is at least 1, not $depth")
      _ <- Either.cond(
        capacity >= depth,
        (),
        s"the capacity, $capacity, is at least the depth, $depth"
      )
    } yield new JitterConfig(depth, capacity)
}
