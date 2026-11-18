package softphone4s.model

/** The `user[:password]` part of a SIP URI; a password cannot exist without a
  * user.
  */
final case class UserInfo(user: User, password: Option[Password])
