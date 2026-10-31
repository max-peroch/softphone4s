package softphone4s.media

import fs2.{Pipe, Stream}
import softphone4s.sdp.Codec

/** An audio codec at work: PCM in, packets of payload out, and back. PCM is
  * 16-bit signed little-endian mono at `pcmRate`; a packet carries 20 ms.
  *
  * This is the plug-in point for codecs of other origin than ours: implement it
  * and give the codec to `SoftphoneStreaming` as an extra, then list its
  * `token` in `codecs`. [[AudioCodec.stateless]] and [[AudioCodec.stateful]]
  * make the pipes from a function over a packet.
  *
  * A codec with memory (the ADPCM ones) keeps it in the pipes it makes, which
  * is why they are made per stream rather than shared.
  */
trait AudioCodec {

  /** The name `codecs` calls it by in the configuration, in lower case. */
  def token: String

  /** How SDP names it. */
  def description: Codec

  /** The sample rate of the PCM it takes and gives, which is the sample rate of
    * the audio device and may differ from the RTP clock rate (G.722).
    */
  def pcmRate: Int

  /** Bytes of PCM in one 20 ms packet. */
  def pcmPacketBytes: Int = pcmRate / 50 * 2

  /** Bytes of payload in one 20 ms packet. */
  def payloadPacketBytes: Int

  /** How much the RTP timestamp moves from one packet to the next: 20 ms at the
    * RTP clock rate.
    */
  def timestampStep: Int = description.clockRate / 50

  /** PCM to payload, one packet's worth at a time. */
  def encoder[F[_]]: Pipe[F, Byte, Byte]

  /** Payload to PCM, a packet's payload at a time. */
  def decoder[F[_]]: Pipe[F, Byte, Byte]
}

object AudioCodec {

  /** Pipes for a codec with memory: `make` is run for each stream and gives the
    * function that turns one chunk into another, which keeps the state of the
    * stream.
    */
  def stateful[F[_]](
      packetBytes: Option[Int],
      make: () => Array[Byte] => Array[Byte]
  ): Pipe[F, Byte, Byte] =
    in =>
      Stream.suspend {
        val step    = make()
        val packets =
          packetBytes.fold(in.chunks)(n => in.chunkN(n, allowFewer = false))
        packets.flatMap(c => Stream.chunk(fs2.Chunk.array(step(c.toArray))))
      }

  /** Pipes for a codec without memory: `f` turns one chunk into another. */
  def stateless[F[_]](
      packetBytes: Int,
      f: Array[Byte] => Array[Byte]
  ): Pipe[F, Byte, Byte] =
    _.chunkN(packetBytes, allowFewer = false)
      .flatMap(c => Stream.chunk(fs2.Chunk.array(f(c.toArray))))
}
