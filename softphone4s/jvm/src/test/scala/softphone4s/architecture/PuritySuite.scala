package softphone4s.architecture

import munit.FunSuite

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** The pure packages stay free of effects (ARCHITECTURE.md, "Why there is no
  * `softphone4s-core`"): they may use `fs2.Chunk`, which is data, but not
  * `cats.effect` or the streams, pipes and I/O of `fs2`. A source scan, since
  * the dependencies of `ip4s` put `cats-effect` on the classpath of every
  * module and the compiler cannot say it.
  */
class PuritySuite extends FunSuite {

  /** The packages that must not touch an effect, and the fewest files each has,
    * so that a scan that finds nothing fails and does not pass.
    */
  private val pure: List[(String, Int)] = List(
    "model"        -> 40,
    "codec"        -> 8,
    "fsm"          -> 15,
    "sdp"          -> 12,
    "srtp"         -> 10,
    "messaging"    -> 3,
    "subscription" -> 14,
    "publication"  -> 2,
    "dialog"       -> 10
  )

  private val effectful =
    """^\s*import\s+(cats\.effect|fs2\.(Stream|Pipe|Compiler|io|concurrent|text|hash)\b|fs2\.\{[^}]*\b(Stream|Pipe)\b)""".r

  /** The source roots of the library, whichever directory the tests run from:
    * what is shared, and what each platform has.
    */
  private val roots: List[Path] =
    for {
      base     <- List("softphone4s", ".")
      platform <- List("shared", "jvm", "js")
      dir = Paths.get(base, platform, "src/main/scala/softphone4s")
      if Files.isDirectory(dir)
    } yield dir

  private def sources(pkg: String): List[Path] = {
    assert(
      roots.nonEmpty,
      "cannot find the sources from " + Paths.get("").toAbsolutePath
    )
    roots
      .map(_.resolve(pkg))
      .filter(Files.isDirectory(_))
      .flatMap(dir =>
        Files
          .walk(dir)
          .iterator()
          .asScala
          .filter(_.toString.endsWith(".scala"))
          .toList
      )
  }

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
              s"$file:${i + 1}: ${line.trim}"
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

  /** pureconfig is for the JVM only. Its readers are in
    * `softphone4s-pureconfig` so that this module can be built for other
    * platforms.
    */
  private val pureconfigImport = """^\s*import\s+pureconfig\b""".r

  test("no source of the library imports pureconfig") {
    val files = sources("")
    assert(files.size >= 300, s"found only ${files.size} files")
    val offenders = files.flatMap { file =>
      Files
        .readAllLines(file)
        .asScala
        .zipWithIndex
        .collect {
          case (line, i) if pureconfigImport.findFirstIn(line).isDefined =>
            s"$file:${i + 1}: ${line.trim}"
        }
    }
    assertEquals(offenders, Nil)
  }

  test("the pureconfig scan recognises what it is looking for") {
    List("import pureconfig.ConfigReader", "import pureconfig.*").foreach(
      line => assert(pureconfigImport.findFirstIn(line).isDefined, line)
    )
    assert(pureconfigImport.findFirstIn("// import pureconfig.*").isEmpty)
  }
}
