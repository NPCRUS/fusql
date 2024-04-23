import scala.annotation.tailrec
import Ast._
import Ast.Symbols._
import Ast.CondOperator._
import Parser._

// TODO: better errors
object Parsers {
  private val functions: Set[String] = Set("count", "concat")

  def parseSeq[T](f: FullParser[T], until: Token): Parser[ErrOr, Seq[T]] = {
    @tailrec
    def inner(rest: Seq[Token], acc: Seq[T]): Either[String, ParserResult[Seq[T]]] = {
      f.apply(rest) match
        case Left(err) => Left(s"Cannot parse sequence: $err")
        case Right(ParserResult(result, Seq())) => Right(ParserResult(acc :+ result, Seq()))
        case Right(ParserResult(result, head +: tail)) if head == until =>
          Right(ParserResult(acc :+ result, tail))
        case Right(ParserResult(result, Coma +: tail)) => inner(tail, acc :+ result)
        case Right(ParserResult(_, head +: tail)) =>
          Left(s"Cannot parse sequence, bumped into ${head.toString}, rest: ${tail.mkString(" ")}")
    }

    Parser.full { tokens =>
      inner(tokens, Seq.empty)
    }

  }

  val booleanLiteralParser: PartialParser[BooleanLiteral] = Parser.partial {
    case StrToken(str) +: tail if str.toBooleanOption.isDefined => ParserResult(BooleanLiteral(str.toBoolean), tail)
  }

  val literalParser: PartialParser[Literal] = Parser.partial {
    case StrToken(s"'$str'") +: tail => ParserResult(StringLiteral(str), tail)
    case StrToken(str) +: tail if str.toIntOption.isDefined => ParserResult(IntLiteral(str.toInt), tail)
  }.orElse(booleanLiteralParser)

  val columnRefParser: PartialParser[ColumnRef] = Parser.partial {
    case StrToken(s"$tableRef.$column") +: tail =>
      ParserResult(ColumnRef(column, Some(tableRef)), tail)
    case StrToken(column) +: tail =>
      ParserResult(ColumnRef(column, None), tail)
  }

  // TODO: support 'arg1 func arg2 ...' notation
  val functionParser: FullParser[FunctionCall] = {
    lazy val argParser = literalParser.orElse(columnRefParser)

    Parser.full {
      case StrToken(func) +: BlockOpen +: tail if functions.contains(func.toLowerCase) =>
        parseSeq(argParser.full("argParser"), BlockClose)(tail).map(v => v.copy(
          result = FunctionCall(func, v.result)
        ))
      case head +: tail => Left(s"Cannot parse function at $head, rest: ${tail.mkString(" ")}")
    }
  }

  lazy val queryParser: FullParser[Query] = Parser.full {
    case Select +: tail =>
      for {
        // select list of columns/references
        selectRefs <- parseSeq[SelectRef](
          columnRefParser.full("columnRef").orElse(aliasParser),
          From
        )(tail)

        // from section
        from <- tableAliasParser.orElse(Parser.full {
          case (s: StrToken) +: tail => Right(ParserResult(s, tail))
          case head +: tail => Left(s"Cannot parse from at $head, rest: ${tail.mkString(" ")}")
        })(selectRefs.tail)

        //
        where <- Parser.partial {
          case Where +: tail => ParserResult(None, tail)
        }.apply(from.tail).map {
          case ParserResult(_, tail) => boolExprParser(tail)
        } match
          case Some(Left(value)) => Left(value)
          case Some(Right(value)) => Right(Some(value))
          case None => Right(None)

        whereTail = where.map(_.tail).getOrElse(from.tail)

      } yield ParserResult(Query(selectRefs.result, from.result, where.map(_.result)), whereTail)
    case head +: tail => Left(s"Cannot parse query at $head, rest: ${tail.mkString(" ")}")
  }

  val expressionParser: FullParser[Expr] = {
    literalParser.full("literal").orElse(functionParser).orElse(queryParser).asInstanceOf[FullParser[Expr]]
  }

  val booleanExprOperandParser: FullParser[BooleanExprOperand] =
    expressionParser.orElse(columnRefParser.full("columnRef"))

  val basicBoolExprParser: FullParser[BasicBoolExpr] =
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
  val betweenParser: FullParser[BetweenExpr] = booleanExprOperandParser.flatMap {
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
  lazy val boolExprParser: FullParser[BoolExpr] = {
    val parser = betweenParser
      .orElse(basicBoolExprParser)
      .orElse(booleanLiteralParser.full("booleanLiteral"))
      .orElse(columnRefParser.full("columnRef"))

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

  val aliasParser: FullParser[Alias] =
    expressionParser.orElse(columnRefParser.full("columnRef")).flatMap {
      case ParserResult(e, As +: StrToken(alias) +: tail) =>
        Right(ParserResult(Alias(e, alias), tail))
      case ParserResult(_, tail) =>
        Left(s"Cannot parse alias at ${tail.mkString(" ")}")
    }

  // TODO: SELECT u.password FROM users as u this case doesn't work
  val tableAliasParser: FullParser[TableAlias] =
    queryParser.orElse(expressionParser).orElse(Parser.partial {
      case (e: StrToken) +: tail => ParserResult(e, tail)
    }.full("strToken")).flatMap {

      case ParserResult(e, As +: StrToken(alias) +: tail) => Right(ParserResult(TableAlias(e, alias), tail))
      case ParserResult(_, tail) => Left(s"Cannot parse tableAlias at ${tail.mkString(" ")}")
    }

  def parse(s: String): Either[String, Query] =
    Preprocessor(s)
      .flatMap(queryParser.apply)
      .map(_.result)
}
