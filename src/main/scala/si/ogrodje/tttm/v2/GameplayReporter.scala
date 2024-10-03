package si.ogrodje.tttm.v2

import zio.ZIO.{logDebug as zLogDebug, logError as zLogError, logInfo as zLogInfo}
import si.ogrodje.tttm.v2.Status.*
import zio.UIO

trait GameplayReporter:
  def logInfo(message: String): UIO[Unit]
  def logDebug(message: String): UIO[Unit]
  def logError(message: String): UIO[Unit]

final case class CLIGameReporter private (gid: GID) extends GameplayReporter:
  override def logInfo(message: String): UIO[Unit]          = zLogInfo(s"[$gid] $message")
  override def logDebug(message: String): UIO[Unit]         = zLogDebug(s"[$gid] $message")
  override def logError(message: ServerEndpoint): UIO[Unit] = zLogError(s"[$gid] $message")

object CLIGameReporter:
  def make(gid: GID): CLIGameReporter = apply(gid)
