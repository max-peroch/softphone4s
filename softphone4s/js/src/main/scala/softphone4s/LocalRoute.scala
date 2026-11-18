package softphone4s

import cats.effect.{Async, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.{IpAddress, Ipv4Address, SocketAddress}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/** The address the OS would send from to reach a remote host. */
private[softphone4s] object LocalRoute {

  @js.native
  @JSImport("node:dgram", JSImport.Namespace)
  private object Dgram extends js.Object {
    def createSocket(kind: String): UdpSocket = js.native
  }

  @js.native
  private trait BoundAddress extends js.Object {
    val address: String = js.native
  }

  @js.native
  private trait UdpSocket extends js.Object {
    def connect(
        port: Int,
        address: String,
        callback: js.Function0[Unit]
    ): Unit =
      js.native
    def once(event: String, listener: js.Function1[js.Error, Unit]): Unit =
      js.native
    def address(): BoundAddress = js.native
    def close(): Unit           = js.native
  }

  /** Connecting a UDP socket only picks the route; nothing is sent. The SDP
    * advertises IN IP4, so only an IPv4 remote has an answer: for an IPv6 one
    * it is none.
    */
  def towards[F[_]: Async](
      remote: SocketAddress[IpAddress]
  ): F[Option[IpAddress]] =
    remote.host.asIpv4 match {
      case None       => Async[F].pure(None)
      case Some(host) =>
        Resource
          .make(Async[F].delay(Dgram.createSocket("udp4")))(socket =>
            Async[F].delay(socket.close()).handleError(_ => ())
          )
          .use(socket =>
            Async[F].async_[Unit] { callback =>
              socket.once(
                "error",
                error => callback(Left(js.JavaScriptException(error)))
              )
              socket.connect(
                remote.port.value,
                host.toString,
                () => callback(Right(()))
              )
            } >> Async[F].delay(
              Ipv4Address.fromString(socket.address().address)
            )
          )
          .map(_.filterNot(_.toString == "0.0.0.0").map(v4 => v4: IpAddress))
    }
}
