package softphone4s.interop

import cats.effect.IO

import scala.sys.process.{Process, ProcessLogger}
import scala.util.control.NonFatal

/** The `docker` command line, which is all the interop tests need of Docker. */
object Docker {

  /** What a command printed (standard output and error together) and how it
    * ended.
    */
  final case class Result(code: Int, output: String) {
    def ok: Boolean         = code == 0
    def text: String        = output.trim
    def lines: List[String] = text.linesIterator.toList
  }

  /** Runs `docker` with `args`. A `docker` that cannot be run at all is a
    * failed result, not an error.
    */
  def run(args: String*): IO[Result] =
    IO.blocking {
      val out    = new StringBuilder
      val logger = ProcessLogger(
        line => { out.append(line).append('\n'); () },
        line => { out.append(line).append('\n'); () }
      )
      try Result(Process("docker" +: args.toList).!(logger), out.toString)
      catch {
        case NonFatal(e) => Result(-1, String.valueOf(e.getMessage))
      }
    }

  /** Whether a Docker daemon answers. */
  def available: IO[Boolean] =
    run("info", "--format", "{{.ServerVersion}}").map(_.ok)
}
