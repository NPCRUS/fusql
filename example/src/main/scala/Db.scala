import slick.jdbc.GetResult
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.PostgresProfile.api.*
import slick.lifted.ProvenShape
import slick.sql.SqlProfile.ColumnOption.{NotNull, Nullable}
import upickle.default.*

import java.sql.{ResultSet, Timestamp}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.collection.SortedMap
import scala.concurrent.Future
import scala.util.Try

object Db {
  val db = Database.forConfig("postgres")

  val libraries: TableQuery[Libraries] = TableQuery[Libraries]

  val oldTimes = LocalDateTime.now().minusHours(10232)

  lazy val setupFuture: Future[Unit] = db.run {
    DBIO.seq(
      libraries.schema.dropIfExists,
      libraries.schema.create,
      libraries ++= Seq(
        Library(0, "fusql", "NPCRUS", "ql,sql,database", None, LocalDateTime.now()),
        Library(0, "cats", "Typelevel", "fp,monads,effects", Some("cats"), oldTimes),
        Library(0, "slick", "nafg", "database,typed,jdbc,fp", None, oldTimes),
        Library(0, "zio", "jdgoes", "fp,monads, fat effect", Some("zio"), oldTimes)
      )
    )
  }

  private def nullify[T](v: T)(converter: T => ujson.Value): ujson.Value = if(v == null) ujson.Null else converter(v)

  def getResultDynamic(fields: Seq[String]): GetResult[ujson.Obj] = GetResult { r =>
    val seq: Seq[(String, ujson.Value)] = fields.map { field =>
      val rs = r.rs
      val idx = rs.findColumn(field)
      val columnType = rs.getMetaData.getColumnTypeName(idx)
      
      val value = columnType match
        case "varchar" => nullify(rs.getString(idx))(ujson.Str.apply)
        case "serial" | "int" => nullify(rs.getInt(idx))(int => ujson.Num.apply(int.doubleValue))
        case "bool" => nullify(rs.getBoolean(idx))(ujson.Bool.apply)
        case "timestamp" => nullify(rs.getTimestamp(idx))(t => ujson.Str.apply(t.toString))
        case _ => ujson.Str(s"cannot parse at pos ${r.currentPos}")
        
      field -> value
    }

    ujson.Obj.from(seq)
  }
}

object Library {
  given ReadWriter[LocalDateTime] = readwriter[String].bimap(
    _.toString,
    v => LocalDateTime.parse(v)
  )
  given  ReadWriter[Library] = macroRW[Library]
}

final case class Library(id: Int,
                   name: String,
                   author: String,
                   tags: String,
                   orga: Option[String],
                   createdAt: LocalDateTime)

class Libraries(tag: Tag) extends Table[Library](tag, "library") {

  def id: Rep[Int] = column[Int]("id", O.PrimaryKey, O.AutoInc)

  def name: Rep[String] = column[String]("name", NotNull)

  def author: Rep[String] = column[String]("author", NotNull)

  def tags: Rep[String] = column[String]("tags", NotNull)

  def orga: Rep[Option[String]] = column[Option[String]]("orga", Nullable)

  def createdAt: Rep[LocalDateTime] = column[LocalDateTime]("created_at", NotNull)

  def * : ProvenShape[Library] =
    (id, name, author, tags, orga, createdAt) <> (Library.apply.tupled, Library.unapply)
}