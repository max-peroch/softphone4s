package softphone4s.opus

import java.nio.file.Path

/** How Opus is run. Speech is coded mono, which is what a phone call is.
  *
  * @param sampleRate
  *   the rate of the PCM, and so of the audio device.
  * @param bitrate
  *   bits a second to aim for, from 6 000 to 510 000; Opus chooses when absent.
  * @param complexity
  *   from 0 (fast) to 10 (best); Opus chooses when absent.
  * @param inbandFec
  *   whether each packet also carries a rougher copy of the one before, to
  *   cover a loss (RFC 6716 §2.1.7).
  * @param expectedPacketLoss
  *   the percent of packets lost that the encoder prepares for; it matters only
  *   with `inbandFec`.
  * @param library
  *   where libopus is; when absent the usual places are tried.
  */
final case class OpusSettings(
    sampleRate: OpusSampleRate = OpusSampleRate.Hz16000,
    bitrate: Option[Int] = Some(24000),
    complexity: Option[Int] = None,
    inbandFec: InbandFec = InbandFec.Enabled,
    expectedPacketLoss: Int = 5,
    library: Option[Path] = None
)

object OpusSettings {

  /** `settings` if Opus allows them. */
  def validated(
      settings: OpusSettings
  ): Either[OpusError.InvalidSettings, OpusSettings] = {
    def check(ok: Boolean, reason: String) =
      Either.cond(ok, (), OpusError.InvalidSettings(reason))
    for {
      _ <- check(
        settings.bitrate.forall(b => b >= 6000 && b <= 510000),
        "the bitrate is from 6000 to 510000"
      )
      _ <- check(
        settings.complexity.forall(c => c >= 0 && c <= 10),
        "the complexity is from 0 to 10"
      )
      _ <- check(
        settings.expectedPacketLoss >= 0 && settings.expectedPacketLoss <= 100,
        "the expected packet loss is a percentage"
      )
    } yield settings
  }
}
