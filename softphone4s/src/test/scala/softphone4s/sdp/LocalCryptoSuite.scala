package softphone4s.sdp

import munit.CatsEffectSuite
import cats.effect.IO
import cats.syntax.all.*
import softphone4s.srtp.{SrtpPolicy, SrtpSuite}

class LocalCryptoSuite extends CatsEffectSuite {

  test("with encryption off there is nothing to make") {
    LocalCrypto
      .generate[IO](SrtpPolicy.Off)
      .map(assertEquals(_, LocalCrypto.Off))
  }

  test("a line for each suite, numbered from 1, each with a key of its own") {
    LocalCrypto.generate[IO](SrtpPolicy.Required).map { crypto =>
      assertEquals(crypto.policy, SrtpPolicy.Required)
      assertEquals(crypto.lines.map(_.tag), List(1, 2))
      assertEquals(crypto.lines.map(_.suite), SrtpSuite.values.toList)
      assertNotEquals(
        crypto.lines(0).master.bytes,
        crypto.lines(1).master.bytes
      )
    }
  }

  test("the keys of one call are not the keys of the next") {
    (
      LocalCrypto.generate[IO](SrtpPolicy.Optional),
      LocalCrypto.generate[IO](SrtpPolicy.Optional)
    ).tupled.map { (a, b) =>
      assertNotEquals(a.lines.head.master.bytes, b.lines.head.master.bytes)
    }
  }

  test("a key is as long as a master key and salt are, and is not all zeros") {
    LocalCrypto.generate[IO](SrtpPolicy.Optional).map { crypto =>
      val bytes = crypto.lines.head.master.bytes
      assertEquals(bytes.size, 30)
      assert(bytes.toArray.exists(_ != 0))
    }
  }
}
