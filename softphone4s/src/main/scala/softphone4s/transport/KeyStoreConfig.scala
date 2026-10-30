package softphone4s.transport

import pureconfig.ConfigReader

import java.nio.file.Path

/** A Java key store on disk.
  *
  * @param storeType
  *   `PKCS12` or `JKS`, as `KeyStore.getInstance` takes it.
  */
final case class KeyStoreConfig(
    path: Path,
    password: String,
    storeType: String = "PKCS12"
) derives ConfigReader
