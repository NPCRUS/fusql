import Parsers.{columnRefParser, parse, queryParser}
import Ast.*

case class TableConfig(tableName: String, forbiddenColumns: Seq[String], contextFilter: Option[BasicBoolExpr]) {
  def forbid(columnName: String): TableConfig = this.copy(forbiddenColumns = this.forbiddenColumns :+ columnName)

  def filter(expr: BasicBoolExpr): TableConfig = this.copy(contextFilter = Some(expr))
}

object Config {
  def empty: Config = Config(Seq.empty)
}

// TODO: model seq as Map[String, TableConfig] here
case class Config(seq: Seq[TableConfig]) {
  def on(tableName: String)(builder: TableConfig => TableConfig): Config =
    Config(
      seq.filter(_.tableName == tableName) :+ builder(TableConfig(tableName, Seq.empty, None))
    )
}

object ConfigApplicator {
  def run(config: Config, input: String): Either[String, Query] =
    parse(input).flatMap { query =>
      check(config, query).map(_ => applyFilter(config, query))
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

    def containsFilteredColumn(columnRef: ColumnRef): Option[String] = columnRef match
      case ColumnRef(column, Some(table)) =>
        val tableFromMap = aliasMap.getOrElse(table, table)
        config.seq.filter(_.tableName == tableFromMap).collectFirst {
          case tc@TableConfig(_, _, Some(cf)) => cf
        }.flatMap { contextFilter =>
          Seq(contextFilter.a, contextFilter.b).collect {
            case cr: ColumnRef => cr
          }.find(_.column == column).map(c => s"Column $column is used for context filtering, remove")
        }
      case ColumnRef(column, None) =>
        config.seq.collect {
          case TableConfig(_, _, Some(contextFilter)) => contextFilter
        }.find { expr =>
          Seq(expr.b, expr.b).collect {
            case cr: ColumnRef => cr
          }.exists(_.column == column)
        }.map(c => s"Column $column is used for context filtering, remove")

    def checkForError(columnRef: ColumnRef): Option[String] =
      isForbidden(columnRef).orElse(containsFilteredColumn(columnRef))
  }

  def check(config: Config, query: Query): Either[String, Acc] = {
    val accOrError = query.from match
      case StrToken(table) => Right(Acc(config, Map(table -> table)))
      case TableAlias(StrToken(table), alias) => Right(Acc(config, Map(table -> table, alias -> table)))
      case TableAlias(_: Expr, alias) => Right(Acc(config, Map.empty))
      case TableAlias(q: Query, alias) => check(config, q)

    for {
      acc <- accOrError
      _ <- checkColumns(query.select, acc)
      _ <- query.where.map(checkWhere(_, acc)).getOrElse(Right(acc))
    } yield acc
  }

  def checkColumns(columns: Seq[SelectRef], acc: Acc): Either[String, Acc] =
    columns.foldLeft[Option[String]](None) { (err, elem) =>
      err match
        case Some(value) => Some(value)
        case None => elem match
          case cr: ColumnRef => acc.isForbidden(cr)
          case Alias(cr: ColumnRef, _) => acc.isForbidden(cr)
          case _ => None
    }.toLeft(acc)

  def checkWhere(where: BoolExpr, acc: Acc): Either[String, Acc] = where match
    case cr: ColumnRef => acc.checkForError(cr).toLeft(acc)
    case BasicBoolExpr(_, a, b) => checkBasicBoolExpr(a, acc).flatMap(_ => checkBasicBoolExpr(b, acc))
    case BetweenExpr(base, a, b) =>
      for {
        _ <- checkBasicBoolExpr(base, acc)
        _ <- checkBasicBoolExpr(a, acc)
        _ <- checkBasicBoolExpr(b, acc)
      } yield acc
    case ComplexBoolExpr(_, a, b) => checkWhere(a, acc).flatMap(_ => checkWhere(b, acc))

  def checkBasicBoolExpr(e: BooleanExprOperand, acc: Acc): Either[String, Acc] = e match
    case cr: ColumnRef =>
      acc.checkForError(cr).toLeft(acc)
    case expr: Expr =>
      checkExpr(expr, acc)

  def checkExpr(e: Expr, acc: Acc): Either[String, Acc] = e match
    case FunctionCall(func, args) => args.foldLeft[Option[String]](None) {
      case (Some(err), _) => Some(err)
      case (None, cr: ColumnRef) => acc.checkForError(cr)
      case _ => None
    }.toLeft(acc)
    case _ => Right(acc)

  def applyFilter(config: Config, query: Query): Query = {
    query.from match
      case TableAlias(q: Query, _) => applyFilter(config, query)
      case TableAlias(StrToken(table), _) => applyContextFilter(config, table, query)
      case StrToken(table) => applyContextFilter(config, table, query)
      case _ => query
  }
  
  def applyContextFilter(config: Config, table: String, query: Query): Query =
    config.seq.find(_.tableName == table).collect {
      case TableConfig(_, _, Some(contextFilter)) => contextFilter
    }.map { contextFilter =>
      query.where match
        case Some(where) => query.copy(where = Some(ComplexBoolExpr(CondOperator.And, contextFilter, where)))
        case None => query.copy(where = Some(contextFilter))
    }.getOrElse(query)
}
