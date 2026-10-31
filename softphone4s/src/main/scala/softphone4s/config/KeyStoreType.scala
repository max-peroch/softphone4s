package softphone4s.config

import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

/** The format of a key store on disk, as `KeyStore.getInstance` names it. */
enum KeyStoreType(val javaName: String) {
  case Pkcs12 extends KeyStoreType("PKCS12")
  case Jks    extends KeyStoreType("JKS")
}

object KeyStoreType {

  /** Read by its name in any case: `pkcs12`, `PKCS12`, `jks`. */
  given ConfigReader[KeyStoreType] = ConfigReader[String].emap(name =>
    values
      .find(_.javaName.equalsIgnoreCase(name.trim))
      .toRight(
        CannotConvert(name, "KeyStoreType", "expected PKCS12 or JKS")
      )
  )
}
