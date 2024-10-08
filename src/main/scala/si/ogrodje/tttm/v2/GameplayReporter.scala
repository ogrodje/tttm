package si.ogrodje.tttm.v2

import zio.ZIO.{logDebug as zLogDebug, logError as zLogError, logInfo as zLogInfo}
import si.ogrodje.tttm.v2.Status.*
import zio.json.*
import zio.{Queue, Task, UIO, ZIO}
import zio.stream.*

trait GameplayReporter:
  def logInfo(message: String)(maybeGid: Option[GID] = None): UIO[Unit]
  def logDebug(message: String)(maybeGid: Option[GID] = None): UIO[Unit]
  def logError(message: String)(maybeGid: Option[GID] = None): UIO[Unit]

final case class CLIGameReporter private (gid: GID) extends GameplayReporter:
  override def logInfo(message: String)(maybeGid: Option[GID] = Some(gid)): UIO[Unit]  = zLogInfo(
    s"[${maybeGid.getOrElse(gid)}] $message"
  )
  override def logDebug(message: String)(maybeGid: Option[GID] = Some(gid)): UIO[Unit] = zLogDebug(
    s"[${maybeGid.getOrElse(gid)}] $message"
  )
  override def logError(message: String)(maybeGid: Option[GID] = Some(gid)): UIO[Unit] = zLogError(
    s"[${maybeGid.getOrElse(gid)}] $message"
  )

object CLIGameReporter:
  def make(gid: GID): CLIGameReporter = apply(gid)

@jsonHintNames(SnakeCase)
@jsonDiscriminator("type")
enum ReporterMessage(message: String, gid: Option[GID]):
  case Info(message: String, gid: Option[GID] = None)  extends ReporterMessage(message, gid)
  case Debug(message: String, gid: Option[GID] = None) extends ReporterMessage(message, gid)
  case Error(message: String, gid: Option[GID] = None) extends ReporterMessage(message, gid)

  def toMessage: String = message
object ReporterMessage:
  given JsonEncoder[ReporterMessage] = DeriveJsonEncoder.gen

final case class StreamingReporter private (queue: Queue[ReporterMessage]) extends GameplayReporter:
  import ReporterMessage.*
  def logInfo(message: String)(maybeGid: Option[GID]): UIO[Unit] =
    queue.offer(Info(message, gid = maybeGid)).unit

  def logDebug(message: String)(maybeGid: Option[GID]): UIO[Unit] =
    queue.offer(Debug(message, gid = maybeGid)).unit

  def logError(message: String)(maybeGid: Option[GID]): UIO[Unit] =
    queue.offer(Error(message, gid = maybeGid)).unit

  def listenWrite(on: ReporterMessage => Task[Unit]): Task[Unit] =
    ZStream
      .fromQueue(queue)
      .tap(msg => on(msg))
      .runDrain

object StreamingReporter:
  def queue: UIO[Queue[ReporterMessage]]                               = Queue.unbounded[ReporterMessage]
  def fromQueue(queue: Queue[ReporterMessage]): UIO[StreamingReporter] = ZIO.succeed(apply(queue))
