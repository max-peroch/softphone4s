package softphone4s.architecture

import munit.FunSuite

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** The pure packages stay free of effects (PLAN.md, "Module layout"): they may
  * use `fs2.Chunk`, which is data, but not `cats.effect` or the streams, pipes
  * and I/O of `fs2`. A source scan, since the dependencies of `ip4s` put
  * `cats-effect` on the classpath of every module and the compiler cannot say
  * it.
  */
class PuritySuite extends FunSuite {

  /** The packages that must not touch an effect, and the fewest files each has,
    * so that a scan that finds nothing fails and does not pass.
    */
  private val pure: List[(String, Int)] = List(
    "model"     -> 40,
    "codec"     -> 8,
    "fsm"       -> 15,
    "sdp"       -> 12,
    "srtp"      -> 10,
    "messaging" -> 3
  )

  private val effectful =
    """^\s*import\s+(cats\.effect|fs2\.(Stream|Pipe|Compiler|io|concurrent|text|hash)\b|fs2\.\{[^}]*\b(Stream|Pipe)\b)""".r

  private def root: Path =
    List("softphone4s/src/main/scala/softphone4s", "src/main/scala/softphone4s")
      .map(Paths.get(_))
      .find(Files.isDirectory(_))
      .getOrElse(
        fail("cannot find the sources from " + Paths.get("").toAbsolutePath)
      )

  private def sources(pkg: String): List[Path] =
    Files
      .walk(root.resolve(pkg))
      .iterator()
      .asScala
      .filter(_.toString.endsWith(".scala"))
      .toList

  pure.foreach { (pkg, atLeast) =>
    test(s"the $pkg package imports no effect, stream or pipe") {
      val files = sources(pkg)
      assert(files.size >= atLeast, s"found only ${files.size} files in $pkg")
      val offenders = files.flatMap { file =>
        Files
          .readAllLines(file)
          .asScala
          .zipWithIndex
          .collect {
            case (line, i) if effectful.findFirstIn(line).isDefined =>
              s"${root.relativize(file)}:${i + 1}: ${line.trim}"
          }
      }
      assertEquals(offenders, Nil)
    }
  }

  test("the scan recognises what it is looking for") {
    List(
      "import cats.effect.IO",
      "import cats.effect.{Async, Ref}",
      "import fs2.Stream",
      "import fs2.{Chunk, Pipe}",
      "import fs2.io.net.Network"
    ).foreach(line => assert(effectful.findFirstIn(line).isDefined, line))
    List(
      "import fs2.Chunk",
      "import cats.syntax.all.*",
      "import cats.data.State"
    )
      .foreach(line => assert(effectful.findFirstIn(line).isEmpty, line))
  }
}
