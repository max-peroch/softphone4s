import scribe.Level
import scribe.format.*
import scribe.output.{BackgroundColoredOutput, Color}
import scribe.output.format.ANSIOutputFormat

object ScribeConfig {

  private def levelBgColoredPaddedRight: FormatBlock = FormatBlock {
    logRecord =>
      val color = logRecord.level match {
        case Level.Trace => Color.White
        case Level.Debug => Color.Green
        case Level.Info  => Color.Blue
        case Level.Warn  => Color.Yellow
        case Level.Error => Color.Red
        case _           => Color.Cyan
      }
      BackgroundColoredOutput(
        color,
        FormatBlock.Level.PaddedRight.format(logRecord)
      )
  }

  def configure(): Unit =
    scribe.Logger.root
      .clearHandlers()
      .withMinimumLevel(scribe.Level.Debug)
      .withHandler(
        formatter =
          formatter"$levelBgColoredPaddedRight ${positionAbbreviated} - ${levelColor(messages)}",
        outputFormat = ANSIOutputFormat
      )
      .replace()
}
