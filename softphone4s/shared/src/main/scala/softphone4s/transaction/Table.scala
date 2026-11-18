package softphone4s.transaction

/** Every running transaction. It is only ever read and replaced by the owner
  * fiber.
  */
private[transaction] final case class Table[F[_]](
    clients: Map[ClientKey, ClientEntry[F]],
    servers: Map[ServerKey, ServerEntry[F]]
)
