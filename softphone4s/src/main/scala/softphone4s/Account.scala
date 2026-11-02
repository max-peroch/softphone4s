package softphone4s

import cats.data.NonEmptyList
import cats.syntax.all.*
import softphone4s.config.AccountConfig
import softphone4s.model.{AccountId, LocalEndpoint, SoftphoneError}

/** An account a `SoftphoneStreaming` holds: its settings under its name. */
private[softphone4s] final case class Account(
    id: AccountId,
    config: AccountConfig
)

private[softphone4s] object Account {

  /** The accounts for `configs`, in order. Each must form a SIP address, and no
    * two may share a user, since an incoming call is told apart by the user in
    * its Request-URI.
    */
  def all(
      configs: NonEmptyList[AccountConfig]
  ): Either[SoftphoneError, NonEmptyList[Account]] =
    for {
      accounts <- configs.traverse(config =>
        LocalEndpoint
          .addressOfRecord(config)
          .map(aor => Account(AccountId(aor), config))
      )
      _ <- accounts.toList
        .groupBy(_.config.user)
        .collectFirst { case (user, held) if held.sizeIs > 1 => user }
        .toLeft(())
        .leftMap(user => SoftphoneError.DuplicateAccount(user.value))
    } yield accounts
}
