package softphone4s.model

/** Digest credentials. Never prints the password. */
final case class Credentials(username: String, password: Secret)
