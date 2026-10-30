package softphone4s.transport

import pureconfig.ConfigReader

/** How TLS connections are made (RFC 3261 §26.2, RFC 5922).
  *
  * @param trustStore
  *   the certificate authorities we trust; the JVM's own when absent.
  * @param clientCertificate
  *   the key and certificate we present if a server asks for one (mutual TLS).
  * @param minVersion
  *   the oldest protocol version we accept.
  * @param cipherSuites
  *   the suites to allow, by Java name; the JVM's defaults when absent.
  * @param hostnameVerification
  *   whether the server's certificate must name the host we meant to reach,
  *   which is taken from the Route or Request-URI of the message being sent. A
  *   destination given as an IP address has no name to check, so with
  *   verification on, a connection to one fails.
  * @param connection
  *   timeouts and size limits, as for TCP.
  */
final case class TlsConfig(
    trustStore: Option[KeyStoreConfig] = None,
    clientCertificate: Option[KeyStoreConfig] = None,
    minVersion: TlsVersion = TlsVersion.Tls12,
    cipherSuites: Option[List[String]] = None,
    hostnameVerification: HostnameVerification = HostnameVerification.Verify,
    connection: StreamConfig = StreamConfig()
) derives ConfigReader
