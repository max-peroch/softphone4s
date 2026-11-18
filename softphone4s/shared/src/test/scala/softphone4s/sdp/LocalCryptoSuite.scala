package softphone4s.sdp

import fs2.Chunk
import munit.FunSuite
import softphone4s.srtp.{MasterKey, SrtpPolicy, SrtpSuite}

class LocalCryptoSuite extends FunSuite {

  private def key(n: Int): MasterKey =
    MasterKey.fromEntropy(Chunk.array(Array.fill[Byte](30)(n.toByte)))

  private val keys = List(key(1), key(2))

  test(
    "with encryption off there is nothing to offer, whatever keys are given"
  ) {
    assertEquals(LocalCrypto.numbered(SrtpPolicy.Off, keys), LocalCrypto.Off)
  }

  test(
    "a line for each suite, numbered from 1 in order, each with its own key"
  ) {
    val crypto = LocalCrypto.numbered(SrtpPolicy.Required, keys)
    assertEquals(crypto.policy, SrtpPolicy.Required)
    assertEquals(crypto.lines.map(_.tag), List(1, 2))
    assertEquals(crypto.lines.map(_.suite), SrtpSuite.values.toList)
    assertEquals(crypto.lines.map(_.master.bytes), keys.map(_.bytes))
  }

  test("a key is needed for each suite") {
    assertEquals(LocalCrypto.keysNeeded, SrtpSuite.values.length)
  }

  test(
    "with fewer keys than suites only the suites that have one are offered"
  ) {
    val crypto = LocalCrypto.numbered(SrtpPolicy.Optional, List(key(1)))
    assertEquals(crypto.lines.map(_.tag), List(1))
  }

  test("a master key is the key and then the salt of the bytes, 30 of them") {
    val bytes = Chunk.array(Array.tabulate[Byte](30)(_.toByte))
    val made  = MasterKey.fromEntropy(bytes)
    assertEquals(made.bytes, bytes)
  }

  test(
    "bytes that fall short are filled out with zeros, so that the key is always whole"
  ) {
    val made = MasterKey.fromEntropy(Chunk.array(Array[Byte](9, 9)))
    assertEquals(made.bytes.size, 30)
    assertEquals(made.bytes.take(3), Chunk.array(Array[Byte](9, 9, 0)))
  }
}
