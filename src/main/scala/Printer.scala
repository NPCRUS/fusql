import Ast._

trait Printer[T] {
  def print(v: T): String
}

object Printer {
  
  extension[T: Printer](value: T) {
    private def print: String = summon[Printer[T]].print(value)
  }

  implicit val literalPrinter: Printer[Literal] = {
    case Ast.BooleanLiteral(value) => value.toString
    case Ast.IntLiteral(value) => value.toString
    case Ast.StringLiteral(value) => s"'$value'"
  }

  implicit val columnRefPrinter: Printer[ColumnRef] = (v: ColumnRef) => v.tableRef match
    case Some(table) => s"$table.${v.column}"
    case None => v.column

  implicit def functionCallPrinter: Printer[FunctionCall] = (v: FunctionCall) => {
    val argStrings = v.args.map {
      case l: Literal => l.print
      case c: ColumnRef => c.print
    }
    s"${v.func}(${argStrings.mkString(" , ")})"
  }
}


