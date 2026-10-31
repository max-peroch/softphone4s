package softphone4s.transport

import pureconfig.ConfigReader
import softphone4s.config.KeyStoreType

import java.nio.file.Path

/** A Java key store on disk. */
final case class KeyStoreConfig(
    path: Path,
    password: String,
    storeType: KeyStoreType = KeyStoreType.Pkcs12
) derives ConfigReader
