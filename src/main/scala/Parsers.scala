import scala.annotation.tailrec
import Ast._
import Ast.Symbols._
import Parser._

// TODO: better errors
object Parsers {

  case class ParserResultOld[+T](result: T, tail: Seq[Token])

  type PartialParserOld[T] = PartialFunction[Seq[Token], ParserResultOld[T]]

  extension [T](p: PartialParserOld[T]) {
    def lift: ParserOld[T] = p andThen Right.apply
  }

  type ParserOld[T] = PartialFunction[Seq[Token], Either[String, ParserResultOld[T]]]

  extension [T](p: ParserOld[T]) {
    def flatMap[B](f: PartialFunction[ParserResultOld[T], Either[String, ParserResultOld[B]]]): ParserOld[B] =
      val t: PartialFunction[Either[String, ParserResultOld[T]], Either[String, ParserResultOld[B]]] = {
        case Right(value) if f.isDefinedAt(value) =>
          f(value)
        case Left(value) =>
          Left(value)
      }

      p andThen t
  }

  private val forbiddenSymbols = Set("insert", "update" , "delete", "truncate", "drop")

  private val functions: Set[String] = Set("count", "concat")

  def splitTokens(s: String): Seq[String] = s.split(" ")

  def splitAndEnrich(s: Seq[String], token: Token): Seq[String] = {
    @tailrec
    def addComa(acc: Seq[String], rest: Seq[String]): Seq[String] = rest match
      case Seq() => acc
      case Seq(last) => acc :+ last
      case head +: tail =>
        addComa(acc.concat(Seq(head, ",")), tail)

    s.flatMap(str => str.split(",").toSeq match
      case Seq(value) if str.contains(',') => Seq(value, ",")
      case seq if str.contains(',') =>
        addComa(Seq.empty, seq)
      case rest => rest
    )
  }

  def filter(tokens: Seq[String]): Either[String, Seq[String]] = forbiddenSymbols.filter(tokens.contains).toList match
    case Nil => Right(tokens)
    case some => Left(s"Bumped into some forbidden keywords: ${some.mkString(",")}")

  def enrichWithToken(token: Symbols)(input: Token): Seq[Token] = input match {
    case StrToken(v) if v.contains(token.repr) =>
      v.split(token.repr.toCharArray.apply(0)).toSeq match
        case head +: Seq() => Seq(StrToken(head), token)
        case seq => seq.flatMap(e => Seq(StrToken(e), token)).dropRight(1)
    case rest => Seq(rest)
  }

  def parseToTokens(tokens: Seq[String]): Seq[Token] = tokens.map { str =>
    Symbols.values.find(_.eq(str)) orElse
      CondOperator.values.find(_.eq(str)) getOrElse
      StrToken(str)
  }
  
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

  val literalParser: PartialParser[Literal] = Parser.partial {
    case StrToken(s"'$str'") +: tail => ParserResult(StringLiteral(str), tail)
    case StrToken(str) +: tail if str.toIntOption.isDefined => ParserResult(IntLiteral(str.toInt), tail)
    case StrToken(str) +: tail if str.toBooleanOption.isDefined => ParserResult(BooleanLiteral(str.toBoolean), tail)
  }

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
    case SELECT +: tail =>
      for {
        // select list of columns/references
        selectRefs <- parseSeq[SelectRef](
          columnRefParser.full("columnRef").orElse(aliasParser),
          FROM
        )(tail)

        // from section
        from <- tableAliasParser.orElse(Parser.full {
          case (s: StrToken) +: tail => Right(ParserResult(s, tail))
          case head +: tail => Left(s"Cannot parse from at $head, rest: ${tail.mkString(" ")}")
        })(selectRefs.tail)

        //
      } yield ParserResult(Query(selectRefs.result, from.result), from.tail)
    case head +: tail => Left(s"Cannot parse query at $head, rest: ${tail.mkString(" ")}")
  }
  
  val expressionParser: FullParser[Expr] = {
    literalParser.full("literal").orElse(functionParser).orElse(queryParser).asInstanceOf[FullParser[Expr]]
  }

  val booleanExprParser: FullParser[BooleanExpr] = {
    val exprOrColumnParser = expressionParser.orElse(columnRefParser.full("columnRef"))

    exprOrColumnParser.flatMap {
      case ParserResult(exprOrColumn, (operator: CondOperator) +: tail) =>
        exprOrColumnParser(tail).map { exprResult =>
          exprResult.copy(result = BooleanExprImpl(operator, exprOrColumn, exprResult.result))
        }
      case ParserResult(result, head +: tail) =>
        Left(s"Cannot parse boolean expr at $head, rest: ${tail.mkString(" ")}")
      case ParserResult(result, _) =>
        Left(s"Cannot parse boolean expr")
    }
  }

  val aliasParser: FullParser[Alias] =
    expressionParser.orElse(columnRefParser.full("columnRef")).flatMap {
      case ParserResult(e, AS +: StrToken(alias) +: tail) =>
        Right(ParserResult(Alias(e, alias), tail))
      case ParserResult(_, tail) =>
        Left(s"Cannot parse alias at ${tail.mkString(" ")}")
    }

  val tableAliasParser: FullParser[TableAlias] =
    queryParser.orElse(expressionParser).flatMap {

      case ParserResult(e, AS +: StrToken(alias) +: tail) => Right(ParserResult(TableAlias(e, alias), tail))
      case ParserResult(_, tail) => Left(s"Cannot parse tableAlias at ${tail.mkString(" ")}")
    }

  def preprocess(s: String): Either[String, Seq[Token]] =
    filter(splitTokens(s))
      .map(parseToTokens)
      .map { seq =>
        seq
          .flatMap(enrichWithToken(Coma))
          .flatMap(enrichWithToken(BlockOpen))
          .flatMap(enrichWithToken(BlockClose))
      }

  def parse(s: String): Either[String, Query] =
    preprocess(s)
      .flatMap(queryParser.apply)
      .map(_.result)
}


/*

  SELECT * FROM table WHERE 1=1 TAKE 10 SKIP 10

 */
