import scala.annotation.tailrec
import Ast._
import Ast.Symbols._
import Ast.CondOperator._
import Parser._

// TODO: better errors
object Parsers {
  private val functions: Set[String] = Set("count", "concat")
  
  val literalParser: Parser[Literal] = Parser.partial("literalParser") {
    case StrToken(s"'$str'") +: tail => ParserResult(StringLiteral(str), tail)
    case StrToken(str) +: tail if str.toIntOption.isDefined => ParserResult(IntLiteral(str.toInt), tail)
    case StrToken(str) +: tail if str.toBooleanOption.isDefined => ParserResult(BooleanLiteral(str.toBoolean), tail)
  }

  val columnRefParser: Parser[ColumnRef] = Parser.partial("columnRefParser") {
    case StrToken(s"$tableRef.$column") +: tail =>
      ParserResult(ColumnRef(column, Some(tableRef)), tail)
    case StrToken(column) +: tail =>
      ParserResult(ColumnRef(column, None), tail)
  }

  // TODO: support 'arg1 func arg2 ...' notation
  val functionParser: Parser[FunctionCall] = {
    lazy val argParser = literalParser.orElse(columnRefParser)

    Parser.partial("functionParser") {
      case StrToken(func) +: BlockOpen +: tail if functions.contains(func.toLowerCase) =>
        ParserResult(func, tail)
    }.andThen(
      Parser.seq(argParser, BlockClose)
    ).map(FunctionCall.apply)
  }

  lazy val queryParser: Parser[Query] = Parser("queryParser") {
    case Select +: tail =>
      for {
        // select list of columns/references
        selectRefs <- Parser.seq[SelectRef](
          aliasParser.orElse(columnRefParser),
          From
        )(tail)

        // from section
        from <- tableAliasParser.orElse(Parser("tableParser") {
          case (s: StrToken) +: tail => Right(ParserResult(s, tail))
          case head +: tail => Left(s"Cannot parse from at $head, rest: ${tail.mkString(" ")}")
        })(selectRefs.tail)

        // where
        where <- Parser.option(Parser.token(Where))(from.tail)(_ => boolExprParser)

      } yield ParserResult(Query(selectRefs.result, from.result, where.result), where.tail)
    case head +: tail =>
      Left(s"Cannot parse query at $head, rest: ${tail.mkString(" ")}")
  }

  val expressionParser: Parser[Expr] = {
    literalParser.orElse(functionParser).orElse(queryParser.enclosed).asInstanceOf[Parser[Expr]]
  }

  val booleanExprOperandParser: Parser[BooleanExprOperand] =
    expressionParser.orElse(columnRefParser)

  val basicBoolExprParser: Parser[BasicBoolExpr] =
    booleanExprOperandParser.andThen(Parser.partial {
      case (operator: CondOperator) +: tail if !operator.isInstanceOf[AndOr] =>
        ParserResult(operator, tail)
    }).andThen(booleanExprOperandParser)
      .map {
        case ((operandA, operator), operandB) =>
          BasicBoolExpr(operator, operandA, operandB)
      }

  val betweenParser: Parser[BetweenExpr] =
    booleanExprOperandParser
      .andThen(Parser.token(Between))
      .andThen(booleanExprOperandParser)
      .andThen(Parser.token(And))
      .andThen(booleanExprOperandParser)
      .map {
        case ((((base, _), a), _), b) =>
          BetweenExpr(base, a, b)
      }

  // What an actual fuck? How does it even work?
  // When parsing multiple nested bool expr for example ((1=1) and id = 0), how can you decide that after second ( 
  // you need to start parsing BasicBoolExpr instead of ComplexBoolExpr
  lazy val boolExprParserIntermediate: Parser[BoolExpr] = {
    val parser = betweenParser
      .orElse(basicBoolExprParser)
      .orElse(columnRefParser)

    parser.flatMap {
      case ParserResult(operandA, And +: tail) =>
        boolExprParserIntermediate.orEnclosed(tail).map { result =>
          result.copy(result = ComplexBoolExpr(And, operandA, result.result))
        }
      case ParserResult(operandA, Or +: tail) =>
        boolExprParserIntermediate.orEnclosed(tail).map { result =>
          result.copy(result = ComplexBoolExpr(Or, operandA, result.result))
        }
      case r@ParserResult(result, tail) => Right(r)
    }
  }

  lazy val boolExprParser: Parser[BoolExpr] =
    boolExprParserIntermediate.orEnclosed.flatMap {
      case ParserResult(operandA, And +: tail) =>
        boolExprParser.orEnclosed(tail).map { result =>
          result.copy(result = ComplexBoolExpr(And, operandA, result.result))
        }
      case ParserResult(operandA, Or +: tail) =>
        boolExprParser.orEnclosed(tail).map { result =>
          result.copy(result = ComplexBoolExpr(Or, operandA, result.result))
        }
      case r@ParserResult(result, tail) => Right(r)
    }

  val aliasPartParser: Parser[String] = Parser.token(As).andThen(Parser.partial {
    case  StrToken(str) +: tail => ParserResult(str, tail)
  }).map(_._2)

  val aliasParser: Parser[Alias] =
    queryParser.orElse(expressionParser).orElse(columnRefParser).andThen(aliasPartParser)
      .map(Alias.apply)

  val tableAliasParser: Parser[TableAlias] =
    queryParser.orElse(expressionParser).orElse(Parser.partial {
      case (e: StrToken) +: tail => ParserResult(e, tail)
    }).andThen(aliasPartParser).map(TableAlias.apply)

  def parse(s: String): Either[String, Query] =
    Preprocessor(s)
      .flatMap(queryParser.apply)
      .map(_.result)
}
