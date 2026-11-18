package softphone4s.subscription

import softphone4s.model.Expires

/** What a NOTIFY that came before the 2xx to the SUBSCRIBE already said. */
final case class Early(standing: Standing, lifetime: Option[Expires])
