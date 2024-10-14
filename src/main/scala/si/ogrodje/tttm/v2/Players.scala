package si.ogrodje.tttm.v2

import eu.timepit.refined.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.api.*
import eu.timepit.refined.predicates.all.NonEmpty
import eu.timepit.refined.generic.*
import eu.timepit.refined.string.*
import zio.*
import zio.http.URL

import java.nio.file.Path
import zio.json.*
import zio.json.yaml.*
import zio.stream.{Stream, ZStream}

import java.io.FileNotFoundException
import scala.io.*
import scala.jdk.StreamConverters.*
import scala.jdk.CollectionConverters.*
import scala.util.Try

type NonEmptyString   = String Refined NonEmpty
type AuthorName       = NonEmptyString
type PlayerServerName = NonEmptyString

final case class Player(
  name: PlayerServerName,
  author: AuthorName,
  @jsonField("author_url") authorURL: URL,
  @jsonField("repository_url") repositoryURL: Option[URL],
  @jsonField("endpoint_url") endpointURL: URL,
  sizes: Set[Size],
  tags: Set[String]
)

object Player:
  given nonEmptyStringDecoder: JsonDecoder[NonEmptyString] =
    JsonDecoder[String].mapOrFail(raw => RefType.applyRef[NonEmptyString](raw))
  given nonEmptyStringEncoder: JsonEncoder[NonEmptyString] =
    JsonEncoder[String].contramap(_.value)

  given sizeDecoder: JsonDecoder[Size] = JsonDecoder[Int].mapOrFail(n => Size.of(n).left.map(_.toString))
  given sizeEncoder: JsonEncoder[Size] = JsonEncoder[Int].contramap(_.value)

  given urlDecoder: JsonDecoder[URL] = JsonDecoder[String].mapOrFail(raw => URL.decode(raw).left.map(_.toString))
  given urlEncoder: JsonEncoder[URL] = JsonEncoder[String].contramap(_.toString)

  given sizesDecoder: JsonDecoder[Set[Size]] =
    JsonDecoder[List[Int]].mapOrFail(
      _.map(n => Size.of(n))
        .foldLeft(Right(Set.empty): Either[String, Set[Size]]) { (agg, c) =>
          c match
            case Left(th)     => Left(s"Invalid number detected - ${th}")
            case Right(value) => agg.map(_ ++ Set(value))
        }
    )

  given playerJsonDecoder: JsonDecoder[Player] = DeriveJsonDecoder.gen[Player]
  given playerJsonEncoder: JsonEncoder[Player] = DeriveJsonEncoder.gen[Player]

final case class PlayersConfig(players: List[Player])

object PlayersConfig:
  given playersJsonDecoder: JsonDecoder[PlayersConfig] = DeriveJsonDecoder.gen
  given playersJsonEncoder: JsonEncoder[PlayersConfig] = DeriveJsonEncoder.gen

  private def readFile(path: Path): String =
    val source = Source.fromFile(path.toFile)
    try
      source.mkString
    finally
      source.close

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
