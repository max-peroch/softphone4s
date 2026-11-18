package softphone4s.transaction

import cats.data.NonEmptyList
import munit.FunSuite
import softphone4s.SipLiterals.*
import softphone4s.model.*
import softphone4s.transaction.TransactionExamples.*

class TransactionKeySuite extends FunSuite {

  test("a response finds its client transaction by branch and CSeq method") {
    val key = ClientKey.forRequest(invite)
    assertEquals(ClientKey.forResponse(response(invite, 486)), key)
    assertEquals(key.map(_.method), Right(SipMethod.Invite))
  }

  test("a CANCEL response does not match the INVITE it cancels") {
    val cancel = request(SipMethod.Cancel)
    assertNotEquals(
      ClientKey.forResponse(response(cancel, 200)),
      ClientKey.forResponse(response(invite, 200))
    )
  }

  test("a different branch is a different transaction") {
    assertNotEquals(
      ClientKey.forRequest(request(SipMethod.Invite, "z9hG4bKone")),
      ClientKey.forRequest(request(SipMethod.Invite, "z9hG4bKtwo"))
    )
  }

  test("an ACK finds the server transaction of the INVITE it acknowledges") {
    val ack = request(SipMethod.Ack)
    assertEquals(ServerKey.forRequest(ack), ServerKey.forRequest(invite))
  }

  test("a CANCEL is a server transaction of its own") {
    assertNotEquals(
      ServerKey.forRequest(request(SipMethod.Cancel)),
      ServerKey.forRequest(invite)
    )
  }

  test("the sent-by of the topmost Via is part of a server key") {
    val other = invite.copy(headers =
      invite.headers.copy(vias =
        NonEmptyList.one(
          via("SIP/2.0/UDP 198.51.100.9:5060;branch=z9hG4bKtxn1")
        )
      )
    )
    assertNotEquals(ServerKey.forRequest(other), ServerKey.forRequest(invite))
  }

  test("only the topmost Via counts") {
    val extra = invite.copy(headers =
      invite.headers.copy(vias =
        NonEmptyList.of(
          invite.headers.topVia,
          via("SIP/2.0/UDP 203.0.113.5;branch=z9hG4bKother")
        )
      )
    )
    assertEquals(ServerKey.forRequest(extra), ServerKey.forRequest(invite))
  }

  test("a Via without a branch cannot be matched") {
    val none = invite.copy(headers =
      invite.headers.copy(vias =
        NonEmptyList.one(via("SIP/2.0/UDP 192.0.2.1:5060"))
      )
    )
    assertEquals(
      ClientKey.forRequest(none),
      Left(TransactionKeyError.MissingBranch)
    )
    assertEquals(
      ServerKey.forRequest(none),
      Left(TransactionKeyError.MissingBranch)
    )
  }

  test("an RFC 2543 branch, without the magic cookie, is not supported") {
    val old = request(SipMethod.Invite, "oldstylebranch")
    ServerKey.forRequest(old) match {
      case Left(TransactionKeyError.Rfc2543Branch(b)) =>
        assertEquals(b.value, "oldstylebranch")
      case other => fail(s"unexpected: $other")
    }
    assert(TransactionKeyError.MissingBranch.message.nonEmpty)
  }
}
