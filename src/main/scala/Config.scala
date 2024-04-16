import Parsers.{parse, queryParser}
import Ast.*

case class TableConfig(tableName: String, forbiddenColumns: Seq[String], contextFilter: Option[BoolExpr]) {
  def forbid(columnName: String): TableConfig = this.copy(forbiddenColumns = this.forbiddenColumns :+ columnName)

  def filter(expr: BoolExpr): TableConfig = this.copy(contextFilter = Some(expr))
}

object Config {
  def empty: Config = Config(Seq.empty)
}

case class Config(seq: Seq[TableConfig]) {
  def on(tableName: String)(builder: TableConfig => TableConfig): Config =
    Config(
      seq.filter(_.tableName == tableName) :+ builder(TableConfig(tableName, Seq.empty, None))
    )
}

object Applicator {
  def run(config: Config, input: String): Either[String, String] =
    parse(input).flatMap { query =>
      checkForbidden(config, query).map(_ => "")
    }

  def checkForbidden(config: Config, query: Query): Either[String, Map[String, String]] = {
    val from = query.from
    val mapper: Either[String, Map[String, String]] = from match
      case StrToken(table) =>
        Right(Map(table -> table))
      case TableAlias(input, alias) => input match
        case StrToken(table) =>
          Right(Map(table -> table, alias -> table))
        case _: Expr => Right(Map.empty)
        case q: Query =>
          checkForbidden(config, q).map(_.values.toList).flatMap {
            case head :: _ =>
              Right(Map(alias -> head, head -> head))
            case _ => Right(Map(alias -> alias))
          }

    def check2(column: String, table: Option[String], mapper: Map[String, String]): Option[String] = {
      val forbiddenColumns = (table match
        case Some(table) =>
          val resolved = mapper.getOrElse(table, table)
          config.seq.filter(_.tableName == resolved)
        case None =>
          config.seq.filter(c => mapper.values.toList.contains(c.tableName))
      ).flatMap(_.forbiddenColumns).toSet

      forbiddenColumns.find(_ == column).map(c => s"column $c is forbidden from querying")
    }

    def check(err: Option[String], tail: Seq[SelectRef], mapper: Map[String, String]): Option[String] = (err, tail) match
      case (Some(err), _) => Some(err)
      case (None, Seq()) => None
      case (None, head +: tail) => head match
        case ColumnRef(column, table) => check2(column, table, mapper)
        case Alias(ColumnRef(column, table), _) => check2(column, table, mapper)
        case _ => None

    mapper.flatMap(m => check(None, query.select, m).toLeft(m))
  }
}
