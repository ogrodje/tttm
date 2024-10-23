package si.ogrodje.tttm.v2

import eu.timepit.refined.api.*
import eu.timepit.refined.predicates.all.NonEmpty
import zio.*
import zio.http.URL
import zio.json.*
import zio.json.yaml.*
import zio.stream.ZStream

import java.nio.file.Path
import scala.io.*
import scala.util.Try

type NonEmptyString   = String Refined NonEmpty
type AuthorName       = NonEmptyString
type PlayerServerName = NonEmptyString

final case class Player(
  name: PlayerServerName,
  author: AuthorName,
  @jsonField("author_url") authorURL: URL,
  @jsonField("endpoint_url") endpointURL: URL,
  sizes: Sizes,
  @jsonField("repository_url") repositoryURL: Option[URL] = None,
  tags: Set[String] = Set.empty
) extends PlayerServer:
  override def id: PlayerServerID             = name.value
  override def serverEndpoint: ServerEndpoint = endpointURL

object Player:
  given nonEmptyStringDecoder: JsonDecoder[NonEmptyString] =
    JsonDecoder[String].mapOrFail(raw => RefType.applyRef[NonEmptyString](raw))
  given nonEmptyStringEncoder: JsonEncoder[NonEmptyString] =
    JsonEncoder[String].contramap(_.value)

  given sizeDecoder: JsonDecoder[Size] = JsonDecoder[Int].mapOrFail(n => Size.safe(n).left.map(_.toString))
  given sizeEncoder: JsonEncoder[Size] = JsonEncoder[Int].contramap(_.value)

  given urlDecoder: JsonDecoder[URL] = JsonDecoder[String].mapOrFail(raw => URL.decode(raw).left.map(_.toString))
  given urlEncoder: JsonEncoder[URL] = JsonEncoder[String].contramap(_.toString)

  given sizesDecoder: JsonDecoder[Sizes] = JsonDecoder[List[Int]].mapOrFail(Sizes.safe)
  given sizesEncoder: JsonEncoder[Sizes] = JsonEncoder[List[Int]].contramap(_.toList.map(_.value))

  given playerJsonDecoder: JsonDecoder[Player] = DeriveJsonDecoder.gen[Player]
  given playerJsonEncoder: JsonEncoder[Player] = DeriveJsonEncoder.gen[Player]

final case class PlayersConfig(players: List[Player])

object PlayersConfig:
  given playersJsonDecoder: JsonDecoder[PlayersConfig] = DeriveJsonDecoder.gen
  given playersJsonEncoder: JsonEncoder[PlayersConfig] = DeriveJsonEncoder.gen

  val empty: PlayersConfig = PlayersConfig(List.empty[Player])

  private def readFile(path: Path): String =
    val source = Source.fromFile(path.toFile)
    try source.mkString
    finally source.close

  private def stringToYaml(content: String) =
    ZIO.fromEither(content.fromYaml[PlayersConfig]).mapError(err => new RuntimeException(err))

  def fromFile(path: Path): ZIO[Any, Throwable, PlayersConfig] =
    ZIO.attemptBlocking(readFile(path)).flatMap(stringToYaml)

  def fromDefaultFile: ZIO[Any, Throwable, PlayersConfig] =
    fromFile(Path.of("players.yml"))

  def readResource(resourcePath: String): ZIO[Any, Throwable, PlayersConfig] =
    val tryRead = ZIO.fromTry(
      Try(scala.io.Source.fromInputStream(getClass.getResourceAsStream(resourcePath)).getLines)
    )

    ZStream
      .fromZIO(tryRead)
      .flatMap(it => ZStream.fromIterator(it))
      .runFold("")(_ + _ + "\n")
      .flatMap(stringToYaml)

  def fromResources: ZIO[Any, Throwable, PlayersConfig] =
    readResource("/players.yml")

  def fromPlayersList(list: List[Player]): PlayersConfig = apply(list)
  def fromPlayers(players: Player*): PlayersConfig       = apply(players.toList)
