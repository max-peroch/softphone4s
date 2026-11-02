package softphone4s.interop

import cats.effect.{IO, Resource}
import com.comcast.ip4s.*
import munit.CatsEffectSuite
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.SoftphoneStreaming
import softphone4s.Softphone
import softphone4s.config.SoftphoneConfig
import softphone4s.model.{Secret, User}

import java.net.DatagramSocket
import javax.sound.sampled.AudioFormat
import scala.concurrent.duration.*

/** The base of the interop tests: an Asterisk to talk to, and a softphone
  * holding the accounts `alice` and `bob` of its configuration. The tests are
  * skipped on a machine without Docker.
  */
abstract class InteropSuite extends CatsEffectSuite {

  override def munitIOTimeout: Duration = 5.minutes

  private val audio = new AudioFormat(8000f, 16, 1, true, false)

  private def user(name: String): User =
    User(name).fold(e => fail(e), identity)

  /** A port nobody is using now, for the softphone's SIP socket. */
  private def freePort: IO[Port] =
    IO.blocking {
      val socket = new DatagramSocket(0)
      try socket.getLocalPort
      finally socket.close()
    }.flatMap(n =>
      IO.fromOption(Port.fromInt(n))(new RuntimeException("no free port"))
    )

  private def requireDocker: IO[Unit] =
    Docker.available.flatMap(up =>
      IO.delay(assume(up, "no Docker daemon is running"))
    )

  /** A softphone registered as nobody yet, with accounts `alice` and `bob`, and
    * the Asterisk it talks to.
    */
  protected def phoneAndServer: Resource[IO, (Softphone[IO], Asterisk)] =
    for {
      _        <- Resource.eval(requireDocker)
      asterisk <- Asterisk.resource
      // Released before the container is removed: if the test failed, what
      // Asterisk logged says what it saw.
      _ <- Resource.makeCase(IO.unit) {
        case (_, Resource.ExitCase.Errored(_)) =>
          asterisk
            .logs(150)
            .flatMap(text =>
              IO.println(s"---- Asterisk log ----\n$text\n---- end ----")
            )
        case _ => IO.unit
      }
      sip <- Resource.eval(freePort)
      rtp <- Resource.eval(freePort)
      config = SoftphoneConfig(
        user = user("alice"),
        password = Secret("alicepw"),
        realm = host"127.0.0.1",
        serverPort = asterisk.sipPort,
        localSipPort = sip,
        baseRtpPort = rtp,
        bindAddress = ip"0.0.0.0",
        additionalAccounts = List(
          softphone4s.config.AccountConfig(
            user("bob"),
            Secret("bobpw"),
            host"127.0.0.1",
            asterisk.sipPort
          )
        )
      )
      phone <- SoftphoneStreaming
        .resource[IO](config, audio, NoOpLogger.impl[IO])
    } yield (phone, asterisk)
}
