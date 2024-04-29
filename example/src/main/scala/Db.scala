import slick.jdbc.GetResult
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.PostgresProfile.api.*
import slick.lifted.ProvenShape
import slick.sql.SqlProfile.ColumnOption.NotNull
import upickle.default.*

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.concurrent.Future

object Db {
  val db = Database.forConfig("postgres")

  val libraries: TableQuery[Libraries] = TableQuery[Libraries]

  val oldTimes = LocalDateTime.now().minusHours(10232)

  lazy val setupFuture: Future[Unit] = db.run {
    DBIO.seq(
      libraries.schema.dropIfExists,
      libraries.schema.create,
      libraries ++= Seq(
        Library(0, "fusql", "NPCRUS", "ql,sql,database", LocalDateTime.now()),
        Library(0, "cats", "Typelevel", "fp,monads,effects", oldTimes),
        Library(0, "slick", "nafg", "database,typed,jdbc,fp", oldTimes)
      )
    )
  }
}

object Library {
  given ReadWriter[LocalDateTime] = readwriter[String].bimap(
    _.toString,
    v => LocalDateTime.parse(v)
  )
  given  ReadWriter[Library] = macroRW[Library]

  given GetResult[Library] = GetResult(r => Library(
    r.nextInt(), r.nextString(), r.nextString(), r.nextString(), LocalDateTime.parse(r.nextString().replace(' ', 'T'))
  ))
}

final case class Library(id: Int,
                   name: String,
                   author: String,
                   tags: String,
                   createdAt: LocalDateTime)

class Libraries(tag: Tag) extends Table[Library](tag, "library") {

  def id: Rep[Int] = column[Int]("id", O.PrimaryKey, O.AutoInc)

  def name: Rep[String] = column[String]("name", NotNull)

  def author: Rep[String] = column[String]("author", NotNull)

  def tags: Rep[String] = column[String]("tags", NotNull)

  def createdAt: Rep[LocalDateTime] = column[LocalDateTime]("created_at", NotNull)

  def * : ProvenShape[Library] =
    (id, name, author, tags, createdAt) <> (Library.apply.tupled, Library.unapply)
}