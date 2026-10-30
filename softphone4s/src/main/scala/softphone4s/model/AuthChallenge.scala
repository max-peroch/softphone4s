package softphone4s.model

import cats.syntax.all.*

/** One challenge of a WWW-Authenticate or Proxy-Authenticate header. Anything
  * other than a well-formed Digest challenge we can answer is `Unsupported`, so
  * a 401 is never lost to an unfamiliar scheme or algorithm.
  */
enum AuthChallenge {
  case Digest(challenge: DigestChallenge)
  case Unsupported(scheme: String, params: String)

  def render: String = this match {
    case Digest(c)               => c.render
    case Unsupported(scheme, ps) => s"$scheme $ps".trim
  }
}

object AuthChallenge {

  /** The challenge to answer: of the Digest challenges we can answer, the one
    * with the strongest algorithm, the first of equals (a server lists its
    * preference first, RFC 7616 §3.7).
    */
  def best(challenges: List[AuthChallenge]): Option[DigestChallenge] =
    challenges
      .collect { case Digest(c) if c.isAnswerable => c }
      .foldLeft(Option.empty[DigestChallenge]) {
        case (Some(kept), c)
            if kept.algorithm.strength >= c.algorithm.strength =>
          Some(kept)
        case (_, c) => Some(c)
      }

  /** Total: never fails, see [[AuthChallenge]]. */
  def parse(value: String): AuthChallenge = {
    val v      = value.trim
    val scheme = v.takeWhile(!_.isWhitespace)
    val rest   = v.drop(scheme.length).trim
    if scheme.equalsIgnoreCase("digest") then
      digest(rest).fold[AuthChallenge](Unsupported(scheme, rest))(Digest(_))
    else Unsupported(scheme, rest)
  }

  private def digest(params: String): Option[DigestChallenge] = {
    val parsed = HeaderSyntax
      .split(params, ',')
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatTraverse { segment =>
        segment.indexOf('=') match {
          case -1  => Some(Nil)
          case idx =>
            unquote(segment.drop(idx + 1).trim)
              .map(value => List(segment.take(idx).trim.toLowerCase -> value))
        }
      }
    parsed.flatMap { entries =>
      val fields = entries.distinctBy(_._1).toMap
      build(fields)
    }
  }

  private def build(fields: Map[String, String]): Option[DigestChallenge] =
    for {
      realm     <- fields.get("realm")
      nonce     <- fields.get("nonce").flatMap(Nonce(_).toOption)
      algorithm <- fields
        .get("algorithm")
        .fold(Option(DigestAlgorithm.Md5))(DigestAlgorithm.fromToken)
    } yield DigestChallenge(
      realm,
      nonce,
      fields.get("opaque"),
      algorithm,
      fields
        .get("qop")
        .fold(Set.empty[Qop])(_.split(',').toList.flatMap(Qop.fromToken).toSet),
      if fields.get("stale").exists(_.equalsIgnoreCase("true")) then
        Staleness.Stale
      else Staleness.Fresh
    )

  /** The text of a quoted-string or token; `None` if a quote is left open. */
  private def unquote(s: String): Option[String] =
    if !s.startsWith("\"") then Some(s)
    else if s.length >= 2 && s.endsWith("\"") then
      Some(UriSyntax.unescape(s.slice(1, s.length - 1)))
    else None
}
