package softphone4s

import softphone4s.model.*

import java.security.MessageDigest

/** What a server does with an Authorization header: recompute the response from
  * the password and compare. Written separately from `DigestAuth` so the tests
  * do not check the code against itself.
  */
object DigestVerifier {

  final case class Verdict(
      valid: Boolean,
      algorithm: String,
      nonce: String,
      nc: Option[String]
  )

  def authorization(request: SipRequest): Option[String] =
    request.headers.others.find(_.name.matches("Authorization")).map(_.value)

  def verify(request: SipRequest, password: String): Option[Verdict] =
    authorization(request).map { header =>
      val params = """(\w+)=(?:"((?:[^"\\]|\\.)*)"|([^,\s]+))""".r
        .findAllMatchIn(header)
        .map(m =>
          m.group(1).toLowerCase -> Option(m.group(2)).getOrElse(m.group(3))
        )
        .toMap
      val algorithm = params.getOrElse("algorithm", "MD5")
      val java      =
        if algorithm.toUpperCase.startsWith("SHA-256") then "SHA-256" else "MD5"
      def h(s: String): String =
        MessageDigest
          .getInstance(java)
          .digest(s.getBytes("UTF-8"))
          .map("%02x".format(_))
          .mkString

      val (user, realm, nonce, uri) =
        (params("username"), params("realm"), params("nonce"), params("uri"))
      val method = request.method.toString.toUpperCase
      val a1     = h(s"$user:$realm:$password")
      val ha1    =
        if algorithm.toLowerCase.endsWith("-sess") then
          h(s"$a1:$nonce:${params("cnonce")}")
        else a1
      val ha2 = params.get("qop") match {
        case Some("auth-int") => h(s"$method:$uri:${h(request.body)}")
        case _                => h(s"$method:$uri")
      }
      val expected = params.get("qop") match {
        case Some(qop) =>
          h(s"$ha1:$nonce:${params("nc")}:${params("cnonce")}:$qop:$ha2")
        case None => h(s"$ha1:$nonce:$ha2")
      }
      Verdict(
        expected == params("response"),
        algorithm,
        nonce,
        params.get("nc")
      )
    }
}
