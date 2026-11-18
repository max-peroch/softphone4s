package softphone4s.media

/** What a jitter buffer has done so far, and the cushion it holds now, in
  * frames.
  */
final case class JitterStats(
    received: Long = 0,
    played: Long = 0,
    lost: Long = 0,
    late: Long = 0,
    duplicates: Long = 0,
    reordered: Long = 0,
    overflows: Long = 0,
    underruns: Long = 0,
    depth: Int = 0
)
