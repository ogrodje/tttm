package si.ogrodje.tttm.v2

import zio.ZIO
import zio.http.URL

import java.net.MalformedURLException

type ServerEndpoint = URL
type PlayerServerID = String

trait PlayerServer:
  def serverEndpoint: ServerEndpoint
  def id: PlayerServerID = serverEndpoint.host.get

final case class ExternalPlayerServer private (serverEndpoint: ServerEndpoint) extends PlayerServer
object ExternalPlayerServer:
  def fromString(raw: String): ZIO[Any, MalformedURLException, ExternalPlayerServer] =
    ZIO.fromEither(URL.decode(raw)).map(apply)

  def unsafeFromURL(url: URL): ExternalPlayerServer = new ExternalPlayerServer(url)
