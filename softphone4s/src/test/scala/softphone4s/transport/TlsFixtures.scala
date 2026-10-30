package softphone4s.transport

import cats.effect.IO
import cats.effect.kernel.Resource
import fs2.io.net.Network
import fs2.io.net.tls.TLSContext

import java.nio.file.{Files, Path}
import java.security.KeyStore
import javax.net.ssl.{KeyManagerFactory, SSLContext, TrustManagerFactory}
import scala.sys.process.*

/** Certificates for the TLS tests, made once with the JDK's `keytool`. */
object TlsFixtures {

  val password = "changeit"

  final case class Files_(
      directory: Path,
      /** A server certificate for `localhost` and 127.0.0.1, and the trust
        * store that holds it.
        */
      server: Path,
      trustsServer: Path,
      /** Another server certificate, that nothing trusts. */
      stranger: Path,
      /** A client certificate, and the trust store that holds it. */
      client: Path,
      trustsClient: Path
  )

  lazy val files: Files_ = {
    val dir     = Files.createTempDirectory("softphone4s-tls")
    val keytool =
      Path.of(System.getProperty("java.home"), "bin", "keytool").toString
    def run(args: String*): Unit = {
      val out    = new StringBuilder
      val status =
        Process(keytool +: args).!(ProcessLogger(out.append(_).append('\n')))
      require(status == 0, s"keytool ${args.mkString(" ")} failed:\n$out")
    }
    def keyPair(name: String, dn: String, san: Option[String]): Path = {
      val store = dir.resolve(s"$name.p12")
      run(
        List(
          "-genkeypair",
          "-alias",
          name,
          "-keyalg",
          "RSA",
          "-keysize",
          "2048",
          "-dname",
          dn,
          "-validity",
          "3650",
          "-keystore",
          store.toString,
          "-storetype",
          "PKCS12",
          "-storepass",
          password,
          "-keypass",
          password
        ) ++ san.toList.flatMap(s => List("-ext", s"SAN=$s"))*
      )
      store
    }
    def trusting(name: String, of: Path): Path = {
      val cert  = dir.resolve(s"$name.cer")
      val trust = dir.resolve(s"trusts-$name.p12")
      run(
        "-exportcert",
        "-alias",
        name,
        "-keystore",
        of.toString,
        "-storepass",
        password,
        "-file",
        cert.toString
      )
      run(
        "-importcert",
        "-alias",
        name,
        "-file",
        cert.toString,
        "-keystore",
        trust.toString,
        "-storetype",
        "PKCS12",
        "-storepass",
        password,
        "-noprompt"
      )
      trust
    }
    val server =
      keyPair("server", "CN=localhost", Some("dns:localhost,ip:127.0.0.1"))
    val stranger =
      keyPair("stranger", "CN=localhost", Some("dns:localhost,ip:127.0.0.1"))
    val client = keyPair("client", "CN=client", None)
    Files_(
      dir,
      server,
      trusting("server", server),
      stranger,
      client,
      trusting("client", client)
    )
  }

  def keyStore(path: Path): KeyStore = {
    val store = KeyStore.getInstance("PKCS12")
    val in    = Files.newInputStream(path)
    try store.load(in, password.toCharArray)
    finally in.close()
    store
  }

  /** The server's side: it shows `identity`, and if `trusting` is given it
    * accepts only clients certified there.
    */
  def serverContext(
      identity: Path,
      trusting: Option[Path] = None
  ): Resource[IO, TLSContext[IO]] =
    Resource.eval(IO.blocking {
      val keys =
        KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
      keys.init(keyStore(identity), password.toCharArray)
      val trust = trusting.map { path =>
        val factory = TrustManagerFactory.getInstance(
          TrustManagerFactory.getDefaultAlgorithm
        )
        factory.init(keyStore(path))
        factory.getTrustManagers
      }
      val context = SSLContext.getInstance("TLS")
      context.init(keys.getKeyManagers, trust.orNull, null)
      Network[IO].tlsContext.fromSSLContext(context)
    })

  def trusting(path: Path): KeyStoreConfig = KeyStoreConfig(path, password)

  /** A client's side: it trusts what `trusts` holds, and shows `identity` if
    * given.
    */
  def clientContext(
      trusts: Path,
      identity: Option[Path] = None
  ): Resource[IO, TLSContext[IO]] =
    Resource.eval(IO.blocking {
      val trust =
        TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
      trust.init(keyStore(trusts))
      val keys = identity.map { path =>
        val factory =
          KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
        factory.init(keyStore(path), password.toCharArray)
        factory.getKeyManagers
      }
      val context = SSLContext.getInstance("TLS")
      context.init(keys.orNull, trust.getTrustManagers, null)
      Network[IO].tlsContext.fromSSLContext(context)
    })
}
