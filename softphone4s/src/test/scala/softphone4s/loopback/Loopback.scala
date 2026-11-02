package softphone4s.loopback

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import org.typelevel.log4cats.noop.NoOpLogger
import softphone4s.{Softphone, SoftphoneStreaming}
import softphone4s.config.{AccountConfig, SoftphoneConfig}
import softphone4s.model.{Callee, Secret, User}

import javax.sound.sampled.AudioFormat
import scala.concurrent.duration.*

/** Two softphones in one process, talking over real UDP on the loopback
  * interface: the stand-in for a remote party that needs no server.
  */
object Loopback {

  private val audio = new AudioFormat(8000f, 16, 1, true, false)

  val AlicePort: Port = port"19420"
  val BobPort: Port   = port"19421"

  /** What the two are configured with, apart from who they are. */
  final case class Settings(
      rtpPairs: Int = 4,
      answerTimeout: FiniteDuration = 60.seconds,
      inviteTimeout: FiniteDuration = 32.seconds,
      bobAlsoHolds: Option[String] = None
  )

  final case class Pair(alice: Softphone[IO], bob: Softphone[IO]) {

    /** Bob's address, as a callee. */
    def callee(user: String = "bob", port: Port = BobPort): Callee =
      Callee(s"$user@127.0.0.1:$port").fold(e => sys.error(e), identity)
  }

  private def config(
      name: String,
      sip: Port,
      rtp: Int,
      settings: Settings
  ): SoftphoneConfig =
    SoftphoneConfig(
      user = User(name).fold(e => sys.error(e), identity),
      password = Secret("secret"),
      realm = host"127.0.0.1",
      localSipPort = sip,
      baseRtpPort = Port.fromInt(rtp).getOrElse(sys.error("port")),
      maxRtpPort =
        Port.fromInt(rtp + settings.rtpPairs * 2 - 1).orElse(Port.fromInt(rtp)),
      bindAddress = ip"127.0.0.1",
      hangupTimeout = 2.seconds,
      answerTimeout = settings.answerTimeout,
      inviteTimeout = settings.inviteTimeout,
      additionalAccounts = Option
        .when(name == "bob")(settings.bobAlsoHolds)
        .flatten
        .map(other =>
          AccountConfig(
            User(other).fold(e => sys.error(e), identity),
            Secret("secret"),
            host"127.0.0.1"
          )
        )
        .toList
    )

  def pair(settings: Settings = Settings()): Resource[IO, Pair] =
    (
      SoftphoneStreaming.resource[IO](
        config("alice", AlicePort, 19430, settings),
        audio,
        NoOpLogger.impl[IO]
      ),
      SoftphoneStreaming.resource[IO](
        config("bob", BobPort, 19450, settings),
        audio,
        NoOpLogger.impl[IO]
      )
    ).mapN(Pair(_, _))
}
