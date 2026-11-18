package softphone4s.sdp

import fs2.Chunk
import softphone4s.srtp.{MasterKey, SrtpSuite}

import java.util.Base64
import scala.util.Try

/** An `a=crypto` line (RFC 4568 §4): a number the answer refers to, the suite,
  * and the master key and salt, inline.
  *
  * A key lifetime is read and not kept: it is a limit of 2^31 packets or so at
  * its largest, more than a call lasts. A master key identifier, and the
  * session parameters that change how keys are used, are not supported, and a
  * line that has one is not usable.
  */
final case class CryptoAttribute(
    tag: Int,
    suite: SrtpSuite,
    master: MasterKey
) {

  def attribute: Attribute =
    Attribute.valued(
      "crypto",
      s"$tag ${suite.sdpName} inline:${Base64.getEncoder.encodeToString(master.bytes.toArray)}"
    )
}

object CryptoAttribute {

  private val Lifetime = """(\d+|2\^\d+)""".r
  private val Window   = """WSH=\d+""".r

  /** The value of an `a=crypto` attribute,
    * `1 AES_CM_128_HMAC_SHA1_80 inline:…`.
    */
  def parse(value: String): Either[CryptoError, CryptoAttribute] =
    value.trim.split("\\s+").toList match {
      case tagText :: suiteName :: keys :: parameters
          if tagText.length <= 9 && tagText.forall(_.isDigit) &&
            tagText.toInt > 0 =>
        for {
          suite <- SrtpSuite
            .fromName(suiteName)
            .toRight(CryptoError.UnknownSuite(suiteName))
          _ <- parameters
            .find(p => !Window.matches(p))
            .fold(Right(()))(p => Left(CryptoError.UnsupportedParameter(p)))
          master <- master(keys)
        } yield CryptoAttribute(tagText.toInt, suite, master)
      case _ => Left(CryptoError.Malformed(value))
    }

  /** The key of `inline:key|lifetime|mki`. */
  private def master(keyParams: String): Either[CryptoError, MasterKey] =
    if !keyParams.startsWith("inline:") || keyParams.contains(';') then
      Left(CryptoError.UnsupportedKeyMethod(keyParams))
    else
      keyParams.drop("inline:".length).split("\\|", -1).toList match {
        case encoded :: rest =>
          for {
            _ <- rest
              .find(_.contains(':'))
              .fold(Right(()))(_ => Left(CryptoError.UnsupportedMki))
            _ <- rest
              .find(p => !Lifetime.matches(p))
              .fold(Right(()))(_ => Left(CryptoError.Malformed(keyParams)))
            bytes <- Try(Base64.getDecoder.decode(encoded)).toEither.left
              .map(_ => CryptoError.Malformed(keyParams))
            key <- MasterKey
              .fromBytes(Chunk.array(bytes))
              .left
              .map(CryptoError.BadKey(_))
          } yield key
        case Nil => Left(CryptoError.Malformed(keyParams))
      }
}
