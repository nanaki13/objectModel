package bon.jo.server
import java.io.InputStream
import java.security.{ KeyStore, SecureRandom }
import javax.net.ssl.{ KeyManagerFactory, SSLContext, TrustManagerFactory }
import akka.actor.typed.ActorSystem
import akka.http.scaladsl.server.{ Directives, Route }
import akka.http.scaladsl.{ ConnectionContext, Http, HttpsConnectionContext }
import scala.concurrent.ExecutionContext
object HttpsConf {

    def apply()(using system: ActorSystem[_]): HttpsConnectionContext =

        given ExecutionContext = system.executionContext

        // Manual HTTPS configuration

        val password: Array[Char] = scala.sys.env.getOrElse("KS_PASS",throw new IllegalStateException("missing KS_PASS env")).toCharArray // do not store passwords in code, read them from somewhere safe!

        val ks: KeyStore = KeyStore.getInstance("PKCS12")
        val keystore: InputStream = getClass.getClassLoader.getResourceAsStream("server.p12")

        require(keystore != null, "Keystore required!")
        ks.load(keystore, password)

        val keyManagerFactory: KeyManagerFactory = KeyManagerFactory.getInstance("SunX509")
        keyManagerFactory.init(ks, password)

        val tmf: TrustManagerFactory = TrustManagerFactory.getInstance("SunX509")
        tmf.init(ks)

        val sslContext: SSLContext = SSLContext.getInstance("TLS")
        sslContext.init(keyManagerFactory.getKeyManagers, tmf.getTrustManagers, new SecureRandom)
        ConnectionContext.httpsServer(sslContext)
}
