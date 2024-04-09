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
  
  def parseSeq_[T](f: Parser[Option, T], until: Token): Parser[ErrOr, Seq[T]] = {
    @tailrec
    def inner(rest: Seq[Token], acc: Seq[T]): Either[String, ParserResult[Seq[T]]] = {
      f.apply(rest) match
        case None => Left(s"Cannot parse sequence: ${rest.mkString(" ")}")
        case Some(ParserResult(result, Seq())) => Right(ParserResult(acc :+ result, Seq()))
        case Some(ParserResult(result, head +: tail)) if head == until =>
          Right(ParserResult(acc :+ result, tail))
        case Some(ParserResult(result, Coma +: tail)) => inner(tail, acc :+ result)
        case Some(ParserResult(_, head +: tail)) =>
          Left(s"Cannot parse sequence, bumped into ${head.toString}, rest: ${tail.mkString(" ")}")
    }

    Parser.full { tokens =>
      inner(tokens, Seq.empty)
    }
    
  }
  
  val literalParser : PartialParser[Literal] = Parser.partial {
    case StrToken(s"'$str'") +: tail => ParserResult(StringLiteral(str), tail)
    case StrToken(str) +: tail if str.toIntOption.isDefined => ParserResult(IntLiteral(str.toInt), tail)
    case StrToken(str) +: tail if str.toBooleanOption.isDefined => ParserResult(BooleanLiteral(str.toBoolean), tail)
  }
  
  val columnRefParser : PartialParser[ColumnRef] = Parser.partial {
    case StrToken(s"$tableRef.$column") +: tail =>
      ParserResult(ColumnRef(column, Some(tableRef)), tail)
    case StrToken(column) +: tail =>
      ParserResult(ColumnRef(column, None), tail)
  }
  
  // TODO: support 'arg1 func arg2 ...' notation
  val functionParser : FullParser[FunctionCall] = {
    lazy val argParser = literalParser.orElse(columnRefParser)
    
    Parser.full {
      case StrToken(func) +: BlockOpen +: tail if functions.contains(func.toLowerCase) =>
        parseSeq_(argParser, BlockClose)(tail).map(v => v.copy(
          result = FunctionCall(func, v.result)
        ))
      case head +: tail => Left(s"Cannot parse function at $head, rest: ${tail.mkString(" ")}")
    }
  }

  val queryParser: FullParser[Query] = Parser.full {
    case seq => Left("")
  }
  
  val expressionParser: FullParser[Expr] = {
    literalParser.full("literal").orElse(functionParser).orElse(queryParser).asInstanceOf[FullParser[Expr]]
  }

  val aliasParser: FullParser[Alias] =
    expressionParser.orElse(columnRefParser.full("columnRef")).flatMap {
      case ParserResult(e, AS +: StrToken(alias) +: tail) => Right(ParserResult(Alias(e, alias), tail))
      case ParserResult(_, tail) => Left(s"Cannot parse alias at ${tail.mkString(" ")}")
    }

  def parseUntilFrom(tokens: Seq[Token]): Either[String, (Seq[StrToken], Seq[Token])] = {
    @tailrec
    def inner(rest: Seq[Token], columns: Seq[StrToken]): Either[String, (Seq[StrToken], Seq[Token])] = rest match
      case Nil if columns.isEmpty => Left(s"There are no columns for SELECT")
      case FROM +: Nil => Left(s"There are no columns for SELECT, next token FROM")
      case FROM +: tail => Right((columns, tail))
      case (head: StrToken) +: StrToken(",") +: tail => inner(tail, columns :+ head)
      case (head: StrToken) +: tail => inner(tail, columns :+ head)
      case rest => Left(s"Got strange sequence of tokens when parsing select: ${rest.mkString(" ")}")

    inner(tokens, Seq.empty)
  }

  def parseFrom(tokens: Seq[Token]): Either[String, StrToken] = tokens match
    case (head: StrToken) +: _ => Right(head)
    case head +: _ => Left(s"Expected table name after FROM, got $head")

  def parseWhere(tokens: Seq[Token]): Either[String, Option[List[Where]]] = tokens match
    case head +: _ if !WHERE.eq(head) => Right(None)
    case _ +: tail =>
      def inner(acc: List[Where], tail: Seq[Token]): Either[String, List[Where]] = tail match
        case Nil => Right(acc)
        case seq => parseCond(seq) match
          case Left(value) => Left(value)
          case Right((cond, Nil)) =>
            Right(acc.appended(WhereAnd(cond)))
          case Right((cond, CondOperator.And +: tail)) => inner(acc.appended(WhereAnd(cond)), tail)
          case Right((cond, CondOperator.Or +: tail)) => inner(acc.appended(WhereOr(cond)), tail)

      inner(List.empty, tail).map(Some.apply)

  def parseCond(tokens: Seq[Token]): Either[String, (Cond, Seq[Token])] =
    tokens match
      case (left: StrToken) +: (c: AndOr) +: (right: StrToken) +: rest if c != CondOperator.Between =>
        Right(SimpleCond(c, left, right), rest)
      case (left: StrToken) +: CondOperator.Between +: (leftBetween: StrToken) +: CondOperator.And +: (rightBetween: StrToken) +: rest =>
        Right(Between(left, leftBetween, rightBetween), rest)
      case seq =>
        Left(s"Got strange sequence of tokens when trying to parse cond: ${seq.mkString(" ")}")

  val queryParserOld: PartialFunction[Seq[Token], Either[String, ParserResultOld[Query]]] = {
    case head +: tail if SELECT.eq(head) =>
      for {
        result <- parseUntilFrom(tail)
        (columns, tail2) = result
        select = Select(columns)

        fromLiteral <- parseFrom(tail2)
        from = From(fromLiteral)

        tail3 = tail2.tail
        where <- parseWhere(tail3)

      } yield ParserResultOld(Query(select, from, where), ???)

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
      .flatMap(queryParserOld orElse {
        case head +: _ => Left(s"Query starts with $head not with ${SELECT}")
      })
      .map(_.result)
}


/*

  SELECT * FROM table WHERE 1=1 TAKE 10 SKIP 10

 */
