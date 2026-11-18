package softphone4s.transport

import fs2.io.file.Path
import softphone4s.config.KeyStoreType
import softphone4s.model.Secret

/** A Java key store on disk. */
final case class KeyStoreConfig(
    path: Path,
    password: Secret,
    storeType: KeyStoreType = KeyStoreType.Pkcs12
)
