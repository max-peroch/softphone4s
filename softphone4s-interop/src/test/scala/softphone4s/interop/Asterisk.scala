package softphone4s.interop

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.Port

import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.UUID
import scala.concurrent.duration.*

/** An Asterisk in a container, configured from `src/test/resources/asterisk`,
  * with its SIP port published on a free port of this machine.
  */
final class Asterisk private (
    name: String,
    val sipPort: Port
) {

  /** What the container has logged lately: with the SIP logger on, the messages
    * it sent and received. For a test that fails to say why.
    */
  def logs(lines: Int): IO[String] =
    Docker.run("logs", "--tail", lines.toString, name).map(_.text)

  /** A command of the Asterisk command line, and what it printed. */
  def cli(command: String): IO[String] =
    Docker
      .run("exec", name, "asterisk", "-rx", command)
      .flatMap(r =>
        if r.ok then IO.pure(r.text)
        else
          IO.raiseError(
            new RuntimeException(s"asterisk -rx '$command': ${r.text}")
          )
      )

  /** Puts `count` messages in the voicemail box of `mailbox` (in the `default`
    * context), which the voicemail application notices within a second and
    * announces to whoever subscribes to the mailbox.
    */
  def leaveVoicemail(
      mailbox: String,
      firstNumber: Int,
      count: Int
  ): IO[Unit] = {
    val inbox = s"/var/spool/asterisk/voicemail/default/$mailbox/INBOX"
    val files = (firstNumber until firstNumber + count).map { n =>
      val base = f"msg$n%04d"
      s"""printf '[message]\\norigmailbox=$mailbox\\ncontext=default\\nexten=s\\npriority=1\\ncallerchan=PJSIP/bob\\ncallerid="Bob" <bob>\\norigtime=1793592000\\nduration=3\\nmsg_id=$n\\n' > $inbox/$base.txt && : > $inbox/$base.wav"""
    }
    val script =
      (s"mkdir -p $inbox" +: files :+ "chown -R asterisk:asterisk /var/spool/asterisk/voicemail")
        .mkString(" && ")
    Docker
      .run("exec", name, "sh", "-c", script)
      .flatMap(r =>
        IO.raiseUnless(r.ok)(
          new RuntimeException(s"leaving voicemail: ${r.text}")
        )
      )
  }

  /** Sets the state of a custom device (`Custom:name`), which a hint of the
    * dialplan turns into presence.
    */
  def setDeviceState(device: String, state: String): IO[Unit] =
    cli(s"devstate change Custom:$device $state").void
}

object Asterisk {

  /** The image the interop tests are pinned to: Asterisk 20, certified release.
    */
  val Image: String = "andrius/asterisk:20.7-cert12_debian-trixie"

  private val Config = List("pjsip.conf", "extensions.conf", "voicemail.conf")

  /** Starts an Asterisk and removes it when the resource is released. Pulls the
    * image if this machine does not have it.
    */
  def resource: Resource[IO, Asterisk] =
    for {
      dir <- tempDir
      name = s"softphone4s-interop-${UUID.randomUUID().toString.take(8)}"
      _ <- Resource.eval(ensureImage)
      _ <- Resource.eval(Config.traverse_(copy(dir, _)))
      _ <- Resource.make(start(name, dir))(_ =>
        Docker.run("rm", "-f", name).void
      )
      port <- Resource.eval(publishedPort(name))
      _    <- Resource.eval(awaitReady(name))
      asterisk = new Asterisk(name, port)
      _ <- Resource.eval(asterisk.cli("pjsip set logger on").void)
    } yield asterisk

  private def tempDir: Resource[IO, Path] =
    Resource.make(
      IO.blocking(Files.createTempDirectory("softphone4s-asterisk"))
    )(dir =>
      IO.blocking {
        Files.list(dir).forEach(Files.deleteIfExists(_))
        Files.deleteIfExists(dir)
      }.void
    )

  private def copy(dir: Path, file: String): IO[Unit] =
    IO.blocking {
      val in = getClass.getResourceAsStream(s"/asterisk/$file")
      try Files.copy(in, dir.resolve(file), StandardCopyOption.REPLACE_EXISTING)
      finally in.close()
    }.void

  private def ensureImage: IO[Unit] =
    Docker.run("image", "inspect", Image).flatMap { present =>
      if present.ok then IO.unit
      else
        Docker
          .run("pull", Image)
          .flatMap(r =>
            IO.raiseUnless(r.ok)(
              new RuntimeException(s"docker pull $Image: ${r.text}")
            )
          )
    }

  private def start(name: String, dir: Path): IO[Unit] = {
    val mounts =
      Config.flatMap(f => List("-v", s"${dir.resolve(f)}:/etc/asterisk/$f:ro"))
    Docker
      .run(
        List(
          "run",
          "-d",
          "--name",
          name,
          "-p",
          "127.0.0.1::5060/udp"
        ) ++ mounts ++
          List(Image)*
      )
      .flatMap(r =>
        IO.raiseUnless(r.ok)(new RuntimeException(s"docker run: ${r.text}"))
      )
  }

  /** The port of this machine that the container's SIP port is published on. */
  private def publishedPort(name: String): IO[Port] = {
    val lookup = Docker.run("port", name, "5060/udp").map { r =>
      r.lines
        .flatMap(_.split(":").lastOption)
        .flatMap(_.trim.toIntOption)
        .flatMap(Port.fromInt)
        .headOption
    }
    // The mapping can take a moment to show after the container has started.
    def loop(left: Int): IO[Port] =
      lookup.flatMap {
        case Some(port)       => IO.pure(port)
        case None if left > 0 => IO.sleep(250.millis) >> loop(left - 1)
        case None             =>
          Docker
            .run("logs", name)
            .flatMap(logs =>
              IO.raiseError(
                new RuntimeException(
                  s"no published SIP port for $name; it logged: ${logs.text.takeRight(500)}"
                )
              )
            )
      }
    loop(40)
  }

  /** Waits until Asterisk has loaded everything and answers its command line.
    */
  private def awaitReady(name: String): IO[Unit] = {
    val ready = Docker
      .run("exec", name, "asterisk", "-rx", "core waitfullybooted")
      .map(_.ok)
    def loop(left: Int): IO[Unit] =
      ready.flatMap {
        case true  => IO.unit
        case false =>
          if left <= 0 then
            IO.raiseError(new RuntimeException(s"$name did not become ready"))
          else IO.sleep(500.millis) >> loop(left - 1)
      }
    loop(120)
  }
}
