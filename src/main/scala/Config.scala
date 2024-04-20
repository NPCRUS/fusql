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
      checkForbiddenInSelect(config, query).map(_ => "")
    }

  case class Acc(config: Config, aliasMap: Map[String, String]) {
    def merge(merger: Map[String, String] => Map[String, String]): Acc =
      this.copy(aliasMap = merger(this.aliasMap))

    def isForbidden(c: ColumnRef): Option[String] = c.tableRef match
      case Some(table) =>
        val tableFromMap = aliasMap.getOrElse(table, table)
        config.seq.find(_.tableName == tableFromMap).flatMap { tableConfig =>
          if(tableConfig.forbiddenColumns.contains(c.column)) {
            Some(s"Column ${c.column} is forbidden on table ${tableConfig.tableName}")
          } else {
            None
          }
        }
      case None =>
        config.seq.filter(c => aliasMap.values.toList.contains(c.tableName))
          .find(_.forbiddenColumns.contains(c.column))
          .map(tableConfig => s"Column ${c.column} is forbidden on table ${tableConfig.tableName}")
  }

  def check(config: Config, query: Query): Either[String, Acc] = {
    val accOrError = query.from match
      case StrToken(table) => Right(Acc(config, Map(table -> table)))
      case TableAlias(StrToken(table), alias) => Right(Acc(config, Map(table -> table, alias -> table)))
      case TableAlias(_: Expr, alias) => Right(Acc(config, Map.empty))
      case TableAlias(q: Query, alias) => check(config, q)

    accOrError.flatMap { acc =>
      query.select.fo
    }
    ???
  }
  
  def checkColumns(columns: Seq[SelectRef], acc: Acc): Either[String, Acc] = columns match
    case Seq() => Right(acc)
    case column +: tail => column match
      
      case cr: ColumnRef => acc.isForbidden(cr).toLeft(acc)

  def checkForbiddenInSelect(config: Config, query: Query): Either[String, Map[String, String]] = {
    val from = query.from
    val mapper: Either[String, Map[String, String]] = from match
      case StrToken(table) =>
        Right(Map(table -> table))
      case TableAlias(input, alias) => input match
        case StrToken(table) =>
          Right(Map(table -> table, alias -> table))
        case _: Expr => Right(Map.empty)
        case q: Query =>
          checkForbiddenInSelect(config, q).map(_.values.toList).flatMap {
            case head :: _ =>
              Right(Map(alias -> head, head -> head))
            case _ => Right(Map(alias -> alias))
          }

    def check(column: String, table: Option[String], mapper: Map[String, String]): Option[String] = {
      val forbiddenColumns = (table match
        case Some(table) =>
          val resolved = mapper.getOrElse(table, table)
          config.seq.filter(_.tableName == resolved)
        case None =>
          config.seq.filter(c => mapper.values.toList.contains(c.tableName))
      ).flatMap(_.forbiddenColumns).toSet

      forbiddenColumns.find(_ == column).map(c => s"column $c is forbidden from querying")
    }

    def fold(err: Option[String], tail: Seq[SelectRef], mapper: Map[String, String]): Option[String] = (err, tail) match
      case (Some(err), _) => Some(err)
      case (None, Seq()) => None
      case (None, head +: tail) => head match
        case ColumnRef(column, table) => check(column, table, mapper)
        case Alias(ColumnRef(column, table), _) => check(column, table, mapper)
        case _ => None

    mapper.flatMap(m => fold(None, query.select, m).toLeft(m))
  }

  // TODO: recursively check nested queries
  def applyContext(query: Query): Either[String, Query] = {
    ???
  }
}
