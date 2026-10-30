package softphone4s.codec

import cats.data.NonEmptyList
import com.comcast.ip4s.*
import org.scalacheck.Gen
import softphone4s.model.*

import scala.collection.immutable.ListMap

/** ScalaCheck generators for valid SIP messages. */
object SipGen extends munit.Assertions {

  private def get[A](e: Either[String, A]): A =
    e.fold(err => fail(err), identity)

  private val alnum: Gen[Char]     = Gen.alphaNumChar
  private val tokenChar: Gen[Char] =
    Gen.frequency(10 -> alnum, 1 -> Gen.oneOf("-.!*_+'~".toList))

  def token(max: Int = 10): Gen[String] =
    Gen.choose(1, max).flatMap(n => Gen.stringOfN(n, tokenChar))

  private def label: Gen[String] =
    Gen.choose(1, 8).flatMap(n => Gen.stringOfN(n, Gen.alphaLowerChar))

  val hostGen: Gen[Host] = Gen.oneOf(
    Gen
      .listOfN(2, label)
      .map(ls =>
        get(Host.fromString(ls.mkString(".") + ".test").toRight("host"))
      ),
    Gen
      .listOfN(4, Gen.choose(0, 255))
      .map(o => get(IpAddress.fromString(o.mkString(".")).toRight("ip"))),
    Gen.const(ip"2001:db8::1")
  )

  val portGen: Gen[Port] =
    Gen.choose(1, 65535).map(p => get(Port.fromInt(p).toRight("port")))

  val userGen: Gen[User] = token().map(s => get(User(s)))

  private val userInfoGen: Gen[UserInfo] =
    for {
      user     <- userGen
      password <- Gen.option(token().map(s => get(Password(s))))
    } yield UserInfo(user, password)

  private val paramsGen: Gen[ListMap[ParamName, Option[ParamValue]]] =
    Gen
      .listOfN(
        2,
        Gen.zip(Gen.oneOf("ttl", "maddr", "user", "x-a"), Gen.option(token()))
      )
      .map { entries =>
        ListMap.from(
          entries
            .distinctBy(_._1)
            .map((n, v) => get(ParamName(n)) -> v.map(x => get(ParamValue(x))))
        )
      }

  val uriGen: Gen[SipUri] =
    for {
      scheme    <- Gen.oneOf(SipScheme.values.toList)
      userInfo  <- Gen.option(userInfoGen)
      host      <- hostGen
      port      <- Gen.option(portGen)
      transport <- Gen.option(Gen.oneOf(Transport.values.toList))
      routing   <- Gen.oneOf(Routing.values.toList)
      params    <- paramsGen
    } yield get(
      SipUri.from(scheme, host, userInfo, port, transport, routing, params)
    )

  val tagGen: Gen[Tag]       = token().map(s => get(Tag(s)))
  val branchGen: Gen[Branch] = token().map(s => get(Branch("z9hG4bK" + s)))
  val callIdGen: Gen[CallId] =
    Gen.zip(token(), hostGen).map((a, h) => get(CallId(s"$a@$h")))

  // Display names exercise quoting: quotes, backslashes, brackets, separators.
  private val displayNameGen: Gen[String] =
    for {
      first <- Gen.alphaNumChar
      rest  <- Gen.listOfN(6, Gen.oneOf("abc XYZ\"\\<>;,:é".toList))
      last  <- Gen.alphaNumChar
    } yield (first :: rest ::: List(last)).mkString

  private val headerParamsGen
      : Gen[ListMap[ParamName, Option[HeaderParamValue]]] =
    Gen
      .listOfN(
        2,
        Gen.zip(
          Gen.oneOf("expires", "q", "x-a"),
          Gen.option(Gen.oneOf("1", "0.5", "\"a;b\"", "tok"))
        )
      )
      .map { entries =>
        ListMap.from(
          entries
            .distinctBy(_._1)
            .map((n, v) =>
              get(ParamName(n)) -> v.map(x => get(HeaderParamValue(x)))
            )
        )
      }

  def nameAddrGen(withTag: Boolean): Gen[NameAddr] =
    for {
      name   <- Gen.option(displayNameGen)
      uri    <- uriGen
      tag    <- if withTag then tagGen.map(Some(_)) else Gen.option(tagGen)
      params <- headerParamsGen
    } yield NameAddr(name, uri, tag, params)

  val viaGen: Gen[Via] =
    for {
      transport <- Gen.oneOf(Transport.values.toList)
      host      <- hostGen
      port      <- Gen.option(portGen)
      branch    <- Gen.option(branchGen)
      rport     <- Gen.option(
        Gen.oneOf(Gen.const(RPort.Requested), portGen.map(RPort.Assigned(_)))
      )
      received <- Gen.option(
        Gen
          .listOfN(4, Gen.choose(0, 255))
          .map(o => get(IpAddress.fromString(o.mkString(".")).toRight("ip")))
      )
      params <- Gen.const(ListMap.empty[ParamName, Option[HeaderParamValue]])
    } yield Via(transport, host, port, branch, rport, received, params)

  private val extensionGen: Gen[ExtensionHeader] =
    for {
      name  <- token(8).map(s => get(FieldName("X-" + s)))
      first <- Gen.alphaNumChar
      rest  <- Gen.listOfN(5, Gen.oneOf("abc 123 :;,=\"é".toList))
      last  <- Gen.alphaNumChar
    } yield ExtensionHeader(name, (first :: rest ::: List(last)).mkString)

  val headersGen: Gen[SipHeaders] =
    for {
      callId <- callIdGen
      from   <- nameAddrGen(withTag = true)
      to     <- nameAddrGen(withTag = false)
      vias   <- Gen.zip(
        viaGen,
        Gen.listOfN(2, viaGen).flatMap(l => Gen.choose(0, 2).map(l.take))
      )
      method   <- Gen.oneOf(SipMethod.values.toList)
      seq      <- Gen.choose(0, Int.MaxValue)
      contacts <- Gen
        .choose(0, 2)
        .flatMap(n => Gen.listOfN(n, nameAddrGen(withTag = false)))
      maxFwd <- Gen.choose(0, 70)
      ctype  <- Gen.option(Gen.oneOf("application/sdp", "text/plain"))
      routes <- Gen
        .choose(0, 2)
        .flatMap(n => Gen.listOfN(n, nameAddrGen(withTag = false)))
      records <- Gen
        .choose(0, 2)
        .flatMap(n => Gen.listOfN(n, nameAddrGen(withTag = false)))
      others <- Gen.choose(0, 3).flatMap(n => Gen.listOfN(n, extensionGen))
    } yield SipHeaders(
      callId = callId,
      from = from,
      to = to,
      vias = NonEmptyList(vias._1, vias._2),
      cseq = CSeq(seq, method),
      contacts = contacts,
      maxForwards = maxFwd,
      contentType = ctype,
      routes = routes,
      recordRoutes = records,
      others = others
    )

  // Includes CR, LF and blank lines, which only Content-Length can delimit.
  val bodyGen: Gen[String] =
    Gen.listOf(Gen.oneOf("abc 0123=\r\n\r\n é€".toList)).map(_.mkString)

  val requestGen: Gen[SipRequest] =
    for {
      method  <- Gen.oneOf(SipMethod.values.toList)
      uri     <- uriGen
      headers <- headersGen
      body    <- bodyGen
    } yield SipRequest(method, uri, headers, body)

  val responseGen: Gen[SipResponse] =
    for {
      code    <- Gen.choose(100, 699)
      reason  <- Gen.listOfN(2, token()).map(_.mkString(" "))
      headers <- headersGen
      body    <- bodyGen
    } yield SipResponse(code, reason, headers, body)

  val messageGen: Gen[SipMessage] = Gen.oneOf(requestGen, responseGen)
}
