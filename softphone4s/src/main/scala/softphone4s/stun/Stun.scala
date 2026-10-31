package softphone4s.stun

import com.comcast.ip4s.{
  IpAddress,
  Ipv4Address,
  Ipv6Address,
  Port,
  SocketAddress
}
import fs2.Chunk

import scala.annotation.tailrec

/** The part of STUN (RFC 8489) a softphone needs: a Binding request, and the
  * address the server saw it come from.
  */
object Stun {

  /** What can be wrong with a datagram that looked like STUN. */
  enum Error {
    case Malformed(reason: String)
    case Rejected(code: Int)
    case NoAddress
  }

  /** The 96 bits that tie a response to its request. */
  final case class TransactionId(bytes: Chunk[Byte])

  /** What a server answered. */
  final case class Response(
      id: TransactionId,
      result: Either[Error, SocketAddress[IpAddress]]
  )

  private val MagicCookie    = 0x2112a442
  private val HeaderLength   = 20
  private val BindingRequest = 0x0001
  private val BindingSuccess = 0x0101
  private val BindingFailure = 0x0111
  private val MappedAddress  = 0x0001
  private val XorMapped      = 0x0020
  private val ErrorCode      = 0x0009

  /** Whether a datagram is STUN rather than SIP: the top two bits of the first
    * byte are zero and the magic cookie follows the length (RFC 8489 §6.3).
    */
  def isStun(bytes: Chunk[Byte]): Boolean =
    bytes.size >= HeaderLength && (bytes(0) & 0xc0) == 0 &&
      int(bytes, 4) == MagicCookie

  /** A Binding request with no attributes. */
  def bindingRequest(id: TransactionId): Chunk[Byte] =
    Chunk.array(
      Array[Byte](
        (BindingRequest >> 8).toByte,
        BindingRequest.toByte,
        0,
        0,
        (MagicCookie >>> 24).toByte,
        (MagicCookie >>> 16).toByte,
        (MagicCookie >>> 8).toByte,
        MagicCookie.toByte
      )
    ) ++ id.bytes

  /** The response in a datagram that [[isStun]]. */
  def decode(bytes: Chunk[Byte]): Either[Error, Response] =
    if !isStun(bytes) then Left(Error.Malformed("not a STUN message"))
    else {
      val kind   = short(bytes, 0)
      val length = short(bytes, 2)
      val id     = TransactionId(part(bytes, 8, HeaderLength))
      if length % 4 != 0 || HeaderLength + length > bytes.size then
        Left(Error.Malformed("bad length"))
      else {
        val attributes =
          attributesOf(part(bytes, HeaderLength, HeaderLength + length), id)
        attributes.flatMap { found =>
          kind match {
            case BindingSuccess =>
              Right(
                Response(
                  id,
                  found
                    .collectFirst { case (XorMapped, address) => address }
                    .orElse(found.collectFirst {
                      case (MappedAddress, address) =>
                        address
                    })
                    .toRight(Error.NoAddress)
                )
              )
            case BindingFailure =>
              Right(
                Response(
                  id,
                  Left(
                    Error.Rejected(
                      errorCodeIn(
                        part(bytes, HeaderLength, HeaderLength + length)
                      )
                    )
                  )
                )
              )
            case other =>
              Left(Error.Malformed(f"unexpected message type 0x$other%04x"))
          }
        }
      }
    }

  /** The address attributes of a message body, with their type. */
  private def attributesOf(
      body: Chunk[Byte],
      id: TransactionId
  ): Either[Error, List[(Int, SocketAddress[IpAddress])]] = {
    @tailrec
    def go(
        rest: Chunk[Byte],
        acc: List[(Int, SocketAddress[IpAddress])]
    ): Either[Error, List[(Int, SocketAddress[IpAddress])]] =
      if rest.isEmpty then Right(acc.reverse)
      else if rest.size < 4 then Left(Error.Malformed("truncated attribute"))
      else {
        val kind   = short(rest, 0)
        val length = short(rest, 2)
        val padded = (length + 3) / 4 * 4
        if rest.size < 4 + length then
          Left(Error.Malformed("truncated attribute"))
        else {
          val value = part(rest, 4, 4 + length)
          val next  = rest.drop(4 + math.min(padded, rest.size - 4))
          kind match {
            case MappedAddress =>
              addressOf(value, None) match {
                case Some(a) => go(next, (kind, a) :: acc)
                case None    => Left(Error.Malformed("bad MAPPED-ADDRESS"))
              }
            case XorMapped =>
              addressOf(value, Some(id)) match {
                case Some(a) => go(next, (kind, a) :: acc)
                case None    => Left(Error.Malformed("bad XOR-MAPPED-ADDRESS"))
              }
            case _ => go(next, acc)
          }
        }
      }
    go(body, Nil)
  }

  /** An address attribute's value; `xor` is the transaction when the address is
    * XOR-mapped.
    */
  private def addressOf(
      value: Chunk[Byte],
      xor: Option[TransactionId]
  ): Option[SocketAddress[IpAddress]] =
    if value.size < 4 then None
    else {
      val family = value(1) & 0xff
      val cookie = Chunk.array(
        Array[Byte](0x21, 0x12, 0xa4.toByte, 0x42)
      )
      val mask = xor.fold(Chunk.empty[Byte])(id => cookie ++ id.bytes)
      def unmask(from: Int, length: Int): Array[Byte] =
        part(value, from, from + length).toArray.zipWithIndex
          .map((b, i) => if xor.isEmpty then b else (b ^ mask(i)).toByte)
      val rawPort =
        xor.fold(short(value, 2))(_ => short(value, 2) ^ (MagicCookie >>> 16))
      for {
        port    <- Port.fromInt(rawPort)
        address <-
          if family == 1 && value.size >= 8 then
            Ipv4Address.fromBytes(unmask(4, 4))
          else if family == 2 && value.size >= 20 then
            Ipv6Address.fromBytes(unmask(4, 16))
          else None
      } yield SocketAddress(address, port)
    }

  private def errorCodeIn(body: Chunk[Byte]): Int = {
    @tailrec
    def go(rest: Chunk[Byte]): Int =
      if rest.size < 4 then 0
      else {
        val length = short(rest, 2)
        if short(rest, 0) == ErrorCode && length >= 4 && rest.size >= 8 then
          (rest(6) & 0x07) * 100 + (rest(7) & 0xff)
        else go(rest.drop(4 + (length + 3) / 4 * 4))
      }
    go(body)
  }

  private def part(bytes: Chunk[Byte], from: Int, until: Int): Chunk[Byte] =
    bytes.drop(from).take(until - from)

  private def short(bytes: Chunk[Byte], at: Int): Int =
    ((bytes(at) & 0xff) << 8) | (bytes(at + 1) & 0xff)

  private def int(bytes: Chunk[Byte], at: Int): Int =
    (short(bytes, at) << 16) | short(bytes, at + 2)
}
