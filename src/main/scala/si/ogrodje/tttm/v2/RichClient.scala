package si.ogrodje.tttm.v2

import zio.ZIO.{from, logInfo, none}
import zio.http.*
import zio.http.netty.NettyConfig
import zio.http.netty.client.NettyClientDriver
import zio.{ZIO, ZLayer}

type RichClient = Client

object RichClient:
  private def readProxyInformation: ZIO[Any, Throwable, Option[URL]] =
    zio.System.env("HTTP_PROXY").flatMap {
      case Some(proxyString) =>
        from(URL.decode(proxyString)).mapBoth(
          e => new Exception(s"Invalid proxy URL: $proxyString", e),
          Some(_)
        )
      case None              => none
    }

  def live: ZLayer[Any, Throwable, RichClient] =
    ZLayer.make[RichClient](
      ZLayer.fromZIO(
        readProxyInformation.tapSome { case Some(proxyUrl) => logInfo(s"Using proxy ${proxyUrl.toString}") }
          .map(maybeProxyURL => ZClient.Config.default.copy(proxy = maybeProxyURL.map(url => Proxy(url = url))))
      ),
      Client.customized,
      NettyClientDriver.live,
      DnsResolver.default,
      ZLayer.succeed(NettyConfig.default)
    )
