package si.ogrodje.tttm.v2.persistance

import com.zaxxer.hikari.HikariConfig
import doobie.*
import doobie.hikari.HikariTransactor
import org.flywaydb.core.Flyway
import org.flywaydb.core.api.configuration.FluentConfiguration
import org.flywaydb.core.api.output.MigrateResult
import zio.interop.catz.*
import zio.{durationInt as _, IO, System, Task, TaskLayer, ZIO, ZLayer, *}

import scala.util.control.NoStackTrace

final case class DBConfiguration private (
  url: String,
  user: String,
  password: String
)

final case class MissingEnvVariable(name: String)
    extends IllegalArgumentException(s"Missing environment variable $name")
    with NoStackTrace

object DBConfiguration:
  def fromEnvironment: IO[Throwable, DBConfiguration] = for
    maybeDatabaseUrl <- System.env("DATABASE_URL")
    databaseUrl      <- ZIO.fromOption(maybeDatabaseUrl).orElseFail(MissingEnvVariable("DATABASE_URL"))
    maybeUser        <- System.env("DATABASE_USER")
    user             <- ZIO.fromOption(maybeUser).orElseFail(MissingEnvVariable("DATABASE_USER"))
    maybePassword    <- System.env("DATABASE_PASSWORD")
    password         <- ZIO.fromOption(maybePassword).orElseFail(MissingEnvVariable("DATABASE_PASSWORD"))
  yield apply(
    databaseUrl,
    user,
    password
  )

  def live: TaskLayer[DBConfiguration] = ZLayer.fromZIO(fromEnvironment)

object DB:
  private val mkFlywayConfiguration: DBConfiguration => FluentConfiguration =
    case DBConfiguration(url, user, password) =>
      Flyway.configure.dataSource(url, user, password)

  private val mkHikariConfig: DBConfiguration => HikariConfig = dbConfig =>
    Class.forName("org.postgresql.Driver")

    val config = new HikariConfig()
    config.setJdbcUrl(dbConfig.url)
    config.setUsername(dbConfig.user)
    config.setPassword(dbConfig.password)
    config.setMaximumPoolSize(2)
    // config.setAutoCommit(false)
    // config.setConnectionTimeout(60 * 1000) // for test
    config

  def loadMigrate(configuration: DBConfiguration): Task[MigrateResult] =
    ZIO.attemptBlocking(mkFlywayConfiguration(configuration).load().migrate())

  def loadMigrate: ZIO[DBConfiguration, Throwable, MigrateResult] = for
    configuration <- ZIO.service[DBConfiguration]
    out           <- loadMigrate(configuration)
  yield out

  type TransactorTask = Transactor[Task]
  object transactor:
    def live: ZLayer[DBConfiguration, Any, TransactorTask] = ZLayer.scoped {
      for
        dbConfig   <- ZIO.service[DBConfiguration]
        hikari      = mkHikariConfig(dbConfig)
        transactor <- HikariTransactor.fromHikariConfig[Task](hikari).toScopedZIO
      yield transactor
    }

    def make(dbConfiguration: DBConfiguration): ZIO[Scope, Throwable, HikariTransactor[Task]] =
      HikariTransactor.fromHikariConfig[Task](mkHikariConfig(dbConfiguration)).toScopedZIO
