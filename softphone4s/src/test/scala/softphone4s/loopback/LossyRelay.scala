package softphone4s.loopback

import cats.effect.std.Supervisor
import cats.effect.{IO, Ref, Resource}
import com.comcast.ip4s.*
import fs2.Chunk
import fs2.io.net.{Datagram, DatagramSocket, Network}

import java.nio.charset.StandardCharsets.US_ASCII
import scala.concurrent.duration.*

/** A UDP relay between the two softphones of a [[Loopback]] pair that loses,
  * duplicates and delays the SIP messages it carries as a plan says.
  *
  * The caller dials the relay's port; the relay passes each datagram on to the
  * callee from another socket, so the callee answers to the relay, which passes
  * the answers back. What follows in a dialog (the ACK of a 2xx, a BYE) goes to
  * the contact addresses, which are the phones' own: the relay is on the path
  * of the call's set-up, which is where the transactions that must survive loss
  * are.
  */
object LossyRelay {

  val CallerSide: Port = port"19460"
  val CalleeSide: Port = port"19461"

  enum Way {
    case ToCallee, ToCaller
  }

  /** What the relay does with a datagram. */
  enum Fault {
    case Pass, Drop, Duplicate
    case Delay(by: FiniteDuration)
  }

  /** A datagram that reached the relay: which way, what it is (`INVITE`, `ACK`,
    * `180`, `200`...) and how many of that kind went that way before it, from 1.
    */
  final case class Seen(way: Way, kind: String, nth: Int)

  type Plan = Seen => Fault

  val passEverything: Plan = _ => Fault.Pass

  final class Relay private[LossyRelay] (
      log: Ref[IO, Vector[Seen]],
      sent: Ref[IO, Vector[Seen]]
  ) {

    /** Every datagram that reached the relay, in order, lost or not. */
    def seen: IO[Vector[Seen]] = log.get

    /** Every datagram the relay passed on, in the order it did: a duplicate
      * twice, a delayed one when its delay is over.
      */
    def delivered: IO[Vector[Seen]] = sent.get

    def count(way: Way, kind: String): IO[Int] =
      seen.map(_.count(s => s.way == way && s.kind == kind))
  }

  /** `INVITE` for a request, `200` for a response. */
  private def kindOf(bytes: Chunk[Byte]): String = {
    val line = new String(bytes.toArray.takeWhile(_ != '\r'.toByte), US_ASCII)
    line.split(' ').toList match {
      case "SIP/2.0" :: status :: _ => status
      case method :: _              => method
      case Nil                      => ""
    }
  }

  private def carry(
      plan: Plan,
      way: Way,
      supervisor: Supervisor[IO],
      log: Ref[IO, Vector[Seen]],
      sent: Ref[IO, Vector[Seen]],
      counts: Ref[IO, Map[(Way, String), Int]],
      from: DatagramSocket[IO],
      out: DatagramSocket[IO],
      to: SocketAddress[IpAddress]
  ): IO[Unit] =
    from.reads
      .evalMap { datagram =>
        val kind = kindOf(datagram.bytes)
        for {
          nth <- counts.modify { m =>
            val n = m.getOrElse((way, kind), 0) + 1
            (m.updated((way, kind), n), n)
          }
          seen = Seen(way, kind, nth)
          send = out.write(Datagram(to, datagram.bytes)) >> sent.update(
            _ :+ seen
          )
          _ <- log.update(_ :+ seen)
          _ <- plan(seen) match {
            case Fault.Pass      => send
            case Fault.Drop      => IO.unit
            case Fault.Duplicate => send >> send
            case Fault.Delay(by) =>
              supervisor.supervise(IO.sleep(by) >> send).void
          }
        } yield ()
      }
      .compile
      .drain

  def resource(plan: Plan): Resource[IO, Relay] =
    for {
      supervisor <- Supervisor[IO](await = false)
      caller     <- Network[IO].bindDatagramSocket(
        address = SocketAddress(ip"127.0.0.1", CallerSide)
      )
      callee <- Network[IO].bindDatagramSocket(
        address = SocketAddress(ip"127.0.0.1", CalleeSide)
      )
      log    <- Resource.eval(Ref.of[IO, Vector[Seen]](Vector.empty))
      sent   <- Resource.eval(Ref.of[IO, Vector[Seen]](Vector.empty))
      counts <- Resource.eval(Ref.of[IO, Map[(Way, String), Int]](Map.empty))
      alice = SocketAddress(ip"127.0.0.1", Loopback.AlicePort)
      bob   = SocketAddress(ip"127.0.0.1", Loopback.BobPort)
      _ <- carry(
        plan,
        Way.ToCallee,
        supervisor,
        log,
        sent,
        counts,
        caller,
        callee,
        bob
      ).background
      _ <- carry(
        plan,
        Way.ToCaller,
        supervisor,
        log,
        sent,
        counts,
        callee,
        caller,
        alice
      ).background
    } yield new Relay(log, sent)
}
