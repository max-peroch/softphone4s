package softphone4s.media

import cats.effect.{Async, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.{IpAddress, Port, SocketAddress}
import fs2.io.net.Network
import softphone4s.model.SoftphoneError

/** The pool of RTP ports of a softphone. A call takes a pair for as long as it
  * lasts ([[allocate]]), and the pair goes back when the call's resources are
  * released. Before a pair is handed out it is tried, so that one another
  * process holds is passed over.
  *
  * @param probe
  *   whether the pair whose RTP port is given can be used.
  * @param attempts
  *   how many pairs are tried before giving up on finding one.
  */
final class RtpPorts[F[_]: Async] private (
    state: Ref[F, RtpPortPool],
    probe: Port => F[Boolean],
    attempts: Int
) {

  /** A pair for as long as the resource is held: its RTP port, whose successor
    * is for RTCP. Fails with
    * [[softphone4s.model.SoftphoneError.RtpPortsExhausted]] if none is free.
    */
  def allocate: Resource[F, Port] =
    Resource.make(acquire)(port => state.update(_.release(port)))

  private def acquire: F[Port] =
    state.get.flatMap(pool => tryEach(pool.range, pool.candidates(attempts)))

  private def tryEach(range: RtpPortRange, candidates: List[Int]): F[Port] =
    candidates match {
      case Nil =>
        Async[F].raiseError(
          SoftphoneError.RtpPortsExhausted(range.first, range.last)
        )
      case index :: rest =>
        range.portOf(index) match {
          case None       => tryEach(range, rest)
          case Some(port) =>
            // Taken before it is tried, so that two calls do not try one pair.
            state
              .modify(pool =>
                pool.reserve(index).fold((pool, false))(_ -> true)
              )
              .flatMap {
                case false => tryEach(range, rest)
                case true  =>
                  probe(port).flatMap {
                    case true  => Async[F].pure(port)
                    case false =>
                      state.update(_.skip(index)) >> tryEach(range, rest)
                  }
              }
        }
    }

  /** How many pairs are in use. */
  def inUse: F[Int] = state.get.map(_.inUse.size)
}

object RtpPorts {

  /** A pool of `range` whose pairs are tried by `probe`. */
  def apply[F[_]: Async](
      range: RtpPortRange,
      probe: Port => F[Boolean],
      attempts: Int = 100
  ): F[RtpPorts[F]] =
    Ref
      .of[F, RtpPortPool](RtpPortPool(range))
      .map(new RtpPorts(_, probe, attempts))

  /** A pool of `range` whose pairs are tried by binding both ports on
    * `address`: it is free if both can be.
    */
  def binding[F[_]: Async: Network](
      range: RtpPortRange,
      address: IpAddress
  ): F[RtpPorts[F]] =
    apply(range, port => bindable(address, port))

  private def bindable[F[_]: Async: Network](
      address: IpAddress,
      port: Port
  ): F[Boolean] =
    Port.fromInt(port.value + 1) match {
      case None       => Async[F].pure(false)
      case Some(next) =>
        (
          Network[F].bindDatagramSocket(address = SocketAddress(address, port)),
          Network[F].bindDatagramSocket(address = SocketAddress(address, next))
        ).tupled.use_.as(true).handleError(_ => false)
    }
}
