package softphone4s.media

import cats.effect.{Resource, Sync}
import fs2.{Chunk, Pipe, Stream}
import softphone4s.sdp.Codec

/** An audio codec at work: PCM in, packets of payload out, and back. PCM is
  * 16-bit signed little-endian mono at `pcmRate`; a packet carries 20 ms, and
  * is as long as the codec makes it, so a codec of variable bit rate can be
  * one.
  *
  * This is the plug-in point for codecs of other origin than ours: implement it
  * and give the codec to `SoftphoneStreaming` as an extra, then list its
  * `token` in `codecs`. [[AudioCodec.encoding]] and [[AudioCodec.decoding]]
  * make the pipes from a function over a packet.
  *
  * A codec with memory, or with native state to release, makes its state when a
  * stream starts and drops it when the stream ends, which is what the
  * [[AudioCodec.Session]] resource of the pipes is for.
  */
trait AudioCodec {

  /** The name `codecs` calls it by in the configuration, in lower case. */
  def token: String

  /** How SDP names it. */
  def description: Codec

  /** The sample rate of the PCM it takes and gives, which is the sample rate of
    * the audio device and may differ from the RTP clock rate (G.722, Opus).
    */
  def pcmRate: Int

  /** Bytes of PCM in one 20 ms packet. */
  def pcmPacketBytes: Int = pcmRate / 50 * 2

  /** How much the RTP timestamp moves from one packet to the next: 20 ms at the
    * RTP clock rate.
    */
  def timestampStep: Int = description.clockRate / 50

  /** PCM to payload: a chunk for each packet, made from `pcmPacketBytes` of
    * PCM.
    */
  def encoder[F[_]: Sync]: Pipe[F, Byte, Chunk[Byte]]

  /** Payload to PCM: a chunk for each packet received. */
  def decoder[F[_]: Sync]: Pipe[F, Chunk[Byte], Byte]
}

object AudioCodec {

  /** What turns one chunk into the next for the length of a stream: it may keep
    * state, which is made when the stream starts and released when it ends.
    */
  type Session = Array[Byte] => Array[Byte]

  /** A session without state. */
  def stateless[F[_]](step: Session): Resource[F, Session] =
    Resource.pure(step)

  /** A session whose state is made for each stream, so that no two streams
    * share it, and which needs nothing releasing.
    */
  def stateful[F[_]: Sync](make: => Session): Resource[F, Session] =
    Resource.eval(Sync[F].delay(make))

  /** The encoder pipe of a codec: PCM cut into packets of `pcmPacketBytes`,
    * each turned into a payload by the session of the stream.
    */
  def encoding[F[_]: Sync](
      pcmPacketBytes: Int,
      session: Resource[F, Session]
  ): Pipe[F, Byte, Chunk[Byte]] =
    in =>
      Stream.resource(session).flatMap { step =>
        in.chunkN(pcmPacketBytes, allowFewer = false)
          .map(pcm => Chunk.array(step(pcm.toArray)))
      }

  /** The decoder pipe: each payload turned into PCM by the session of the
    * stream.
    */
  def decoding[F[_]: Sync](
      session: Resource[F, Session]
  ): Pipe[F, Chunk[Byte], Byte] =
    in =>
      Stream.resource(session).flatMap { step =>
        in.flatMap(payload => Stream.chunk(Chunk.array(step(payload.toArray))))
      }
}
