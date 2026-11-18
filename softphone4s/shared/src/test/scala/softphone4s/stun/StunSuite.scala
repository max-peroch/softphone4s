package softphone4s.stun

import com.comcast.ip4s.*
import fs2.Chunk
import munit.FunSuite

class StunSuite extends FunSuite {

  private def bytes(hex: String): Chunk[Byte] =
    Chunk.array(
      hex.split("\\s+").filter(_.nonEmpty).map(Integer.parseInt(_, 16).toByte)
    )

  private val id =
    Stun.TransactionId(bytes("b7 e7 a7 01 bc 34 d6 86 fa 87 df ae"))

  /** The IPv4 response of RFC 5769 §2.2: a software attribute, an XOR-mapped
    * address, a message integrity and a fingerprint.
    */
  private val rfc5769 = bytes(
    """01 01 00 3c 21 12 a4 42 b7 e7 a7 01 bc 34 d6 86 fa 87 df ae
      |80 22 00 0b 74 65 73 74 20 76 65 63 74 6f 72 20
      |00 20 00 08 00 01 a1 47 e1 12 a6 43
      |00 08 00 14 2b 91 f5 99 fd 9e 90 c3 8c 74 89 f9 2a f9 ba 53 f0 6b e7 d7
      |80 28 00 04 c0 7d 4c 96""".stripMargin
  )

  test("a Binding request is the 20-byte header with the transaction id") {
    assertEquals(
      Stun.bindingRequest(id),
      bytes("00 01 00 00 21 12 a4 42 b7 e7 a7 01 bc 34 d6 86 fa 87 df ae")
    )
  }

  test(
    "the address in the response of RFC 5769 is read, other attributes skipped"
  ) {
    assertEquals(
      Stun.decode(rfc5769),
      Right(
        Stun.Response(id, Right(SocketAddress(ip"192.0.2.1", port"32853")))
      )
    )
  }

  test("a plain MAPPED-ADDRESS is read when there is no XOR one") {
    val response = bytes(
      "01 01 00 0c 21 12 a4 42 b7 e7 a7 01 bc 34 d6 86 fa 87 df ae 00 01 00 08 00 01 0d 05 c6 33 64 07"
    )
    assertEquals(
      Stun.decode(response).map(_.result),
      Right(Right(SocketAddress(ip"198.51.100.7", port"3333")))
    )
  }

  test("an error response is the code the server gave") {
    val response = bytes(
      "01 11 00 08 21 12 a4 42 b7 e7 a7 01 bc 34 d6 86 fa 87 df ae 00 09 00 04 00 00 04 14"
    )
    assertEquals(
      Stun.decode(response),
      Right(Stun.Response(id, Left(Stun.Error.Rejected(420))))
    )
  }

  test("a success with no address is an error") {
    val response = bytes(
      "01 01 00 00 21 12 a4 42 b7 e7 a7 01 bc 34 d6 86 fa 87 df ae"
    )
    assertEquals(
      Stun.decode(response),
      Right(Stun.Response(id, Left(Stun.Error.NoAddress)))
    )
  }

  test(
    "an address nothing is reached at is a bad message: port 0, unspecified, a group"
  ) {
    List(
      SocketAddress(ip"203.0.113.7", port"0"),
      SocketAddress(ip"0.0.0.0", port"41000"),
      SocketAddress(ip"224.0.0.1", port"41000"),
      SocketAddress(ip"::", port"41000"),
      SocketAddress(ip"ff02::1", port"41000")
    ).foreach(address =>
      assertEquals(
        Stun.decode(StunServer.response(id.bytes, address)),
        Left(Stun.Error.Malformed("bad XOR-MAPPED-ADDRESS")),
        address.toString
      )
    )
    // and an ordinary one, loopback and IPv6 included, is read
    List(
      SocketAddress(ip"127.0.0.1", port"41000"),
      SocketAddress(ip"2001:db8::1", port"41000")
    ).foreach(address =>
      assertEquals(
        Stun.decode(StunServer.response(id.bytes, address)),
        Right(Stun.Response(id, Right(address)))
      )
    )
  }

  test("a length that runs past the datagram is malformed") {
    val cut = rfc5769.take(40)
    assert(Stun.decode(cut).isLeft)
  }

  test("SIP is not taken for STUN") {
    val sip = Chunk.array("OPTIONS sip:a@b SIP/2.0\r\nVia: x\r\n\r\n".getBytes)
    assert(!Stun.isStun(sip))
    assert(Stun.isStun(rfc5769))
  }
}
