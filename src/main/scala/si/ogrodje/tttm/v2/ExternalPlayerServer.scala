package si.ogrodje.tttm.v2

import zio.http.URL

type ServerEndpoint = String
trait PlayerServer:
  def serverEndpoint: ServerEndpoint
  val show: String = serverEndpoint

final case class ExternalPlayerServer private (serverEndpoint: ServerEndpoint) extends PlayerServer
object ExternalPlayerServer:
  def fromString(raw: String) = new ExternalPlayerServer(raw)
  def fromURL(url: URL)       = new ExternalPlayerServer(url.toString)
