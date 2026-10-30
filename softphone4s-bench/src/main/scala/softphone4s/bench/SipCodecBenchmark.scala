package softphone4s.bench

import fs2.Chunk
import org.openjdk.jmh.annotations.*
import softphone4s.codec.{SipCodec, SipParser}
import softphone4s.model.SipMessage

import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.TimeUnit

/** Parse, frame and encode throughput on realistic messages. Run with
  * `softphone4sBench/Jmh/run -prof gc` to see allocation per operation.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
class SipCodecBenchmark {

  private val sdp = Seq(
    "v=0",
    "o=- 3857290155 3857290155 IN IP4 192.168.1.10",
    "s=softphone4s",
    "c=IN IP4 192.168.1.10",
    "t=0 0",
    "m=audio 8000 RTP/AVP 0 101",
    "a=rtpmap:0 PCMU/8000",
    "a=rtpmap:101 telephone-event/8000",
    "a=fmtp:101 0-15",
    "a=sendrecv",
    ""
  ).mkString("\r\n")

  private val inviteText = Seq(
    "INVITE sip:bob@sip.example.com SIP/2.0",
    "Via: SIP/2.0/UDP 192.168.1.10:5060;branch=z9hG4bK776asdhds;rport",
    "Max-Forwards: 70",
    "From: \"Alice\" <sip:alice@sip.example.com>;tag=1928301774",
    "To: <sip:bob@sip.example.com>",
    "Call-ID: a84b4c76e66710@192.168.1.10",
    "CSeq: 314159 INVITE",
    "Contact: <sip:alice@192.168.1.10:5060>",
    "Allow: INVITE, ACK, CANCEL, OPTIONS, BYE",
    "User-Agent: softphone4s",
    "Content-Type: application/sdp",
    s"Content-Length: ${sdp.getBytes(UTF_8).length}",
    "",
    sdp
  ).mkString("\r\n")

  private val okText = Seq(
    "SIP/2.0 200 OK",
    "Via: SIP/2.0/UDP 192.168.1.10:5060;branch=z9hG4bK776asdhds;received=192.168.1.10",
    "From: <sip:alice@sip.example.com>;tag=1928301774",
    "To: <sip:bob@sip.example.com>;tag=a6c85cf",
    "Call-ID: a84b4c76e66710@192.168.1.10",
    "CSeq: 314160 BYE",
    "Content-Length: 0",
    "",
    ""
  ).mkString("\r\n")

  private val invite: Chunk[Byte] = Chunk.array(inviteText.getBytes(UTF_8))
  private val ok: Chunk[Byte]     = Chunk.array(okText.getBytes(UTF_8))

  private def message(bytes: Chunk[Byte]): SipMessage =
    SipParser.parse(bytes).fold(err => sys.error(err.message), identity)

  private val inviteMessage: SipMessage = message(invite)
  private val okMessage: SipMessage     = message(ok)

  @Benchmark def parseInvite()
      : Either[softphone4s.codec.ParseError, SipMessage] =
    SipParser.parse(invite)

  @Benchmark def parseOk(): Either[softphone4s.codec.ParseError, SipMessage] =
    SipParser.parse(ok)

  @Benchmark def frameInvite(): softphone4s.codec.Framing =
    SipParser.frame(invite)

  @Benchmark def encodeInvite(): Chunk[Byte] =
    SipCodec.encode(inviteMessage)

  @Benchmark def encodeOk(): Chunk[Byte] =
    SipCodec.encode(okMessage)

  @Benchmark def roundTripInvite()
      : Either[softphone4s.codec.ParseError, SipMessage] =
    SipCodec.decode(SipCodec.encode(inviteMessage))
}
