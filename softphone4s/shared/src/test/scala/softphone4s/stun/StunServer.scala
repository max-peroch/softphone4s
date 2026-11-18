package softphone4s.stun

import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.Chunk
import fs2.io.net.{Datagram, Network}

/** A STUN server for tests: it tells whoever asks that they are at `says`
  * (their own address when absent), and ignores the first `ignore` requests.
  */
object StunServer {

  final case class Running(
      address: SocketAddress[IpAddress],
      requests: Ref[IO, Int]
  )

  def apply(
      says: Option[SocketAddress[IpAddress]] = None,
      ignore: Int = 0
  ): Resource[IO, Running] =
    for {
      socket <- Network[IO].bindDatagramSocket(
        address = SocketAddress(ip"127.0.0.1", port"0")
      )
      at       <- Resource.eval(addressOf(socket.address))
      requests <- Resource.eval(Ref.of[IO, Int](0))
      _        <- socket.reads
        .evalMap { datagram =>
          requests.getAndUpdate(_ + 1).flatMap { before =>
            val id   = Chunk.array(datagram.bytes.drop(8).take(12).toArray)
            val seen = says.getOrElse(datagram.remote)
            socket
              .write(Datagram(datagram.remote, response(id, seen)))
              .whenA(before >= ignore)
          }
        }
        .compile
        .drain
        .background
    } yield Running(at, requests)

  private def addressOf(
      address: GenSocketAddress
  ): IO[SocketAddress[IpAddress]] =
    address match {
      case a: SocketAddress[?] =>
        a.host match {
          case ip: IpAddress => IO.pure(SocketAddress(ip, a.port))
          case _ => IO.raiseError(new RuntimeException("not an IP address"))
        }
      case _ => IO.raiseError(new RuntimeException("not an IP address"))
    }

  /** A success response with the address XOR-mapped (RFC 8489 §14.2). */
  def response(id: Chunk[Byte], seen: SocketAddress[IpAddress]): Chunk[Byte] = {
    val cookie = Array[Byte](0x21, 0x12, 0xa4.toByte, 0x42)
    val port   = seen.port.value ^ 0x2112
    val octets = seen.host.toBytes
    val mask   = cookie ++ id.toArray
    val family = if octets.length == 4 then 1 else 2
    val value  =
      Array[Byte](0, family.toByte, (port >> 8).toByte, port.toByte) ++
        octets.zipWithIndex.map((b, i) => (b ^ mask(i)).toByte)
    val attribute = Array[Byte](0x00, 0x20, 0, value.length.toByte) ++ value
    Chunk.array(
      Array[Byte](
        0x01,
        0x01,
        0,
        attribute.length.toByte
      ) ++ cookie ++ id.toArray ++ attribute
    )
  }
}
