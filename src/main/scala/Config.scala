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
      check(config, query).map(_ => "")
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
    case cr: ColumnRef => acc.isForbidden(cr).toLeft(acc)
    case BasicBoolExpr(_, a, b) => checkBasicBoolExpr(a, acc).flatMap(_ => checkBasicBoolExpr(b, acc))
    case Between(base, a, b) =>
      for {
        _ <- checkBasicBoolExpr(base, acc)
        _ <- checkBasicBoolExpr(a, acc)
        _ <- checkBasicBoolExpr(b, acc)
      } yield acc
    case ComplicatedBoolExpr(_, a, b) => checkWhere(a, acc).flatMap(_ => checkWhere(b, acc))

  def checkBasicBoolExpr(e: BooleanExprOperand, acc: Acc): Either[String, Acc] = e match
    case cr: ColumnRef =>
      acc.isForbidden(cr).toLeft(acc)
    case expr: Expr =>
      checkExpr(expr, acc)

  def checkExpr(e: Expr, acc: Acc): Either[String, Acc] = e match
    case FunctionCall(func, args) => args.foldLeft[Option[String]](None) {
      case (Some(err), _) => Some(err)
      case (None, cr: ColumnRef) => acc.isForbidden(cr)
      case _ => None
    }.toLeft(acc)
    case _ => Right(acc)
}
