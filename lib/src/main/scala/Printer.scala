import Ast._

trait Printer[T] {
  def print(v: T): String
}

object Printer {
  
  extension[T: Printer](value: T) {
    private def print: String = summon[Printer[T]].print(value)
  }

  given strTokenPrinter: Printer[StrToken] = _.v

  given seqPrinter[T: Printer]: Printer[Seq[T]] = seq =>
    seq.map(_.print).mkString(" , ")

  given literalPrinter: Printer[Literal] = {
    case Ast.BooleanLiteral(value) => value.toString
    case Ast.IntLiteral(value) => value.toString
    case Ast.StringLiteral(value) => s"'$value'"
  }

  given columnRefPrinter: Printer[ColumnRef] = (v: ColumnRef) => v.tableRef match
    case Some(table) => s"$table.${v.column}"
    case None => v.column

  given functionCallPrinter: Printer[FunctionCall] = (func: FunctionCall) => {
    lazy val argPrinter: Printer[Literal | ColumnRef] = {
      case e: Literal => literalPrinter.print(e)
      case e: ColumnRef => columnRefPrinter.print(e)
    }

    s"${func.func}(${func.args.print(using seqPrinter(using argPrinter))})"
  }

  given expressionPrinter: Printer[Expr] = {
    case e: Literal => e.print
    case e: FunctionCall => e.print
    case e: Query => e.print
  }

  given booleanExprOperandPrinter: Printer[BooleanExprOperand] = {
    case e: Expr => e.print
    case e: ColumnRef => e.print
  }

  given basicBooleanExprPrinter: Printer[BasicBoolExpr] = expr =>
    s"${expr.a.print} ${expr.operator.repr.toUpperCase} ${expr.b.print}"

  given betweenPrinter: Printer[BetweenExpr] = v => s"${v.base.print} BETWEEN ${v.a.print} AND ${v.b.print}"

  given boolExprPrinter: Printer[BoolExpr] = {
    case e: ColumnRef => e.print
    case e: BasicBoolExpr => e.print
    case e: BetweenExpr => e.print
    case e: ComplexBoolExpr => e.print
  }

  given andOrPrinter: Printer[AndOr] = {
    case e: CondOperator => e.toString.toUpperCase
  }

  given complexBoolExprPrinter: Printer[ComplexBoolExpr] = v => {
    val a = v.a match
      case e: ComplexBoolExpr => s"(${e.print})"
      case other => other.print

    val b = v.b match
      case e: ComplexBoolExpr => s"(${e.print})"
      case other => other.print

    s"$a ${v.operator.print} $b"
  }

  given exprColumnRefPrinter: Printer[Expr | ColumnRef] = {
    case e: Expr => e.print
    case e: ColumnRef => e.print
  }
  
  given aliasPrinter: Printer[Alias] = v => s"${v.input.print(using exprColumnRefPrinter)} AS ${v.alias}"
  
  given exprQueryStrTokenPrinter: Printer[Expr | StrToken] = {
    case e: Expr => e.print
    case e: StrToken => e.print
  }
  
  given tableAliasPrinter: Printer[TableAlias] = v => s"${v.input.print} AS ${v.alias}"

  given strTokenTableAliasPrinter: Printer[StrToken | TableAlias] = {
    case e: StrToken => e.print
    case e: TableAlias => e.print
  }
  
  given queryPrinter: Printer[Query] = (v: Query) => {
    lazy val selectRefPrinter: Printer[SelectRef] = {
      case e: ColumnRef => e.print
      case e: Alias => e.print
    }

    val selectStr = v.select.print(using seqPrinter(using selectRefPrinter))

    val wherePrinter: Printer[Option[BoolExpr]] = {
      case None => ""
      case Some(v) => s"WHERE ${v.print}"
    }

    s"SELECT $selectStr FROM ${v.from.print} ${v.where.print(using wherePrinter)}"
  }
}


