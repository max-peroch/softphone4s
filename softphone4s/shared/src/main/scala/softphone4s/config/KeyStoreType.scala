package softphone4s.config

/** The format of a key store on disk, as `KeyStore.getInstance` names it. */
enum KeyStoreType(val javaName: String) {
  case Pkcs12 extends KeyStoreType("PKCS12")
  case Jks    extends KeyStoreType("JKS")
}

object KeyStoreType {

  /** Read by its name in any case: `pkcs12`, `PKCS12`, `jks`. */

}
