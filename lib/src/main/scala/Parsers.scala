import scala.annotation.tailrec
import Ast._
import Ast.Symbols._
import Ast.CondOperator._
import Parser._

// TODO: better errors
object Parsers {
  private val functions: Set[String] = Set("count", "concat")

  def parseSeq[T](f: Parser[T], until: Token): Parser[Seq[T]] = {
    @tailrec
    def inner(rest: Seq[Token], acc: Seq[T]): Either[String, ParserResult[Seq[T]]] = {
      f.apply(rest) match
        case Left(err) =>
          Left(s"Cannot parse sequence: $err")
        case Right(ParserResult(result, Seq())) =>
          Right(ParserResult(acc :+ result, Seq()))
        case Right(ParserResult(result, head +: tail)) if head == until =>
          Right(ParserResult(acc :+ result, tail))
        case Right(ParserResult(result, Coma +: tail)) =>
          inner(tail, acc :+ result)
        case Right(ParserResult(_, head +: tail)) =>
          Left(s"Cannot parse sequence, bumped into ${head.toString}, rest: ${tail.mkString(" ")}")
    }

    Parser("parseSeq") { tokens =>
      inner(tokens, Seq.empty)
    }

  }

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

    Parser("functionParser") {
      case StrToken(func) +: BlockOpen +: tail if functions.contains(func.toLowerCase) =>
        parseSeq(argParser, BlockClose)(tail).map(v => v.copy(
          result = FunctionCall(func, v.result)
        ))
      case head +: tail => Left(s"Cannot parse function at $head, rest: ${tail.mkString(" ")}")
    }
  }

  lazy val queryParser: Parser[Query] = Parser("queryParser") {
    case Select +: tail =>
      for {
        // select list of columns/references
        selectRefs <- parseSeq[SelectRef](
          columnRefParser.orElse(aliasParser),
          From
        )(tail)

        // from section
        from <- tableAliasParser.orElse(Parser("tableParser") {
          case (s: StrToken) +: tail => Right(ParserResult(s, tail))
          case head +: tail => Left(s"Cannot parse from at $head, rest: ${tail.mkString(" ")}")
        })(selectRefs.tail)

        //
        where <- from.tail match
          case Where +: tail =>
            boolExprParser(tail).map(Some.apply)
          case _ => 
            Right(None)

        whereTail = where.map(_.tail).getOrElse(from.tail)

      } yield ParserResult(Query(selectRefs.result, from.result, where.map(_.result)), whereTail)
    case head +: tail => 
      Left(s"Cannot parse query at $head, rest: ${tail.mkString(" ")}")
  }

  val expressionParser: Parser[Expr] = {
    literalParser.orElse(functionParser).orElse(queryParser.enclosed).asInstanceOf[Parser[Expr]]
  }

  val booleanExprOperandParser: Parser[BooleanExprOperand] =
    expressionParser.orElse(columnRefParser)

  val basicBoolExprParser: Parser[BasicBoolExpr] =
    booleanExprOperandParser.flatMap {
      case ParserResult(operandAResult, (operator: CondOperator) +: tail) if !operator.isInstanceOf[AndOr] =>
        booleanExprOperandParser(tail).map { operandBResult =>
          operandBResult.copy(result = BasicBoolExpr(operator, operandAResult, operandBResult.result))
        }
      case ParserResult(result, head +: tail) =>
        Left(s"Cannot parse boolean expr at $head, rest: ${tail.mkString(" ")}")
      case ParserResult(result, _) =>
        Left(s"Cannot parse boolean expr")
    }

  // TODO: repetitive left returning
  val betweenParser: Parser[BetweenExpr] = booleanExprOperandParser.flatMap {
    case ParserResult(base, Between +: tail) =>
      booleanExprOperandParser(tail).flatMap {
        case ParserResult(operand1, And +: tail) =>
          booleanExprOperandParser(tail).map { result =>
            result.copy(result = BetweenExpr(base, operand1, result.result))
          }
        case ParserResult(result, head +: tail) =>
          Left(s"Cannot parse between expr at $head, rest: ${tail.mkString(" ")}")
        case ParserResult(result, _) =>
          Left(s"Cannot parse between expr")
      }
    case ParserResult(result, head +: tail) =>
      Left(s"Cannot parse between expr at $head, rest: ${tail.mkString(" ")}")
    case ParserResult(result, _) =>
      Left(s"Cannot parse between expr")
  }

  // TODO: cover WHERE (....) OR/AND (....) ...
  lazy val boolExprParser: Parser[BoolExpr] = {
    val parser = betweenParser
      .orElse(basicBoolExprParser)
      .orElse(columnRefParser)

    parser.flatMap {
      case ParserResult(operandA, And +: tail) =>
        boolExprParser(tail).map { result =>
          result.copy(result = ComplicatedBoolExpr(And, operandA, result.result))
        }
      case ParserResult(operandA, Or +: tail) =>
        boolExprParser(tail).map { result =>
          result.copy(result = ComplicatedBoolExpr(Or, operandA, result.result))
        }
      case r@ParserResult(result, tail) => Right(r)
    }
  }

  val aliasParser: Parser[Alias] =
    expressionParser.orElse(columnRefParser).flatMap {
      case ParserResult(e, As +: StrToken(alias) +: tail) =>
        Right(ParserResult(Alias(e, alias), tail))
      case ParserResult(_, tail) =>
        Left(s"Cannot parse alias at ${tail.mkString(" ")}")
    }

  // TODO: SELECT u.password FROM users as u this case doesn't work
  val tableAliasParser: Parser[TableAlias] =
    queryParser.orElse(expressionParser).orElse(Parser.partial {
      case (e: StrToken) +: tail => ParserResult(e, tail)
    }).flatMap {
      case ParserResult(e, As +: StrToken(alias) +: tail) => Right(ParserResult(TableAlias(e, alias), tail))
      case ParserResult(_, tail) => Left(s"Cannot parse tableAlias at ${tail.mkString(" ")}")
    }

  def parse(s: String): Either[String, Query] =
    Preprocessor(s)
      .flatMap(queryParser.apply)
      .map(_.result)
}
