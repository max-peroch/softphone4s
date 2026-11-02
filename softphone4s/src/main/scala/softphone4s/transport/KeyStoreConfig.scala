package softphone4s.transport

import pureconfig.ConfigReader
import softphone4s.config.KeyStoreType
import softphone4s.model.Secret

import java.nio.file.Path

/** A Java key store on disk. */
final case class KeyStoreConfig(
    path: Path,
    password: Secret,
    storeType: KeyStoreType = KeyStoreType.Pkcs12
) derives ConfigReader
