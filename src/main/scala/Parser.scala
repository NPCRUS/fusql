import scala.annotation.tailrec
import Ast._

object Parser {

  case class ParserResult[+T](result: T, tail: Seq[Token])

  type PartialParser[T] = PartialFunction[Seq[Token], ParserResult[T]]

  extension [T](p: PartialParser[T]) {
    def lift: Parser[T] = p andThen Right.apply
  }

  type Parser[T] = PartialFunction[Seq[Token], Either[String, ParserResult[T]]]

  extension [T](p: Parser[T]) {
    def flatMap[B](f: PartialFunction[ParserResult[T], Either[String, ParserResult[B]]]): Parser[B] =
      val t: PartialFunction[Either[String, ParserResult[T]], Either[String, ParserResult[B]]] = {
        case Right(value) if f.isDefinedAt(value) =>
          f(value)
        case Left(value) =>
          Left(value)
      }

      p andThen t
  }

  private val forbiddenSymbols = Set("insert", "update" , "delete", "truncate")

  def splitTokens(s: String): Seq[String] = s.split(" ")

  def splitComas(s: Seq[String]): Seq[String] = {
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

  def parseToTokens(tokens: Seq[String]): Seq[Token] = tokens.map { str =>
    Symbols.values.find(_.eq(str)) orElse
      CondOperator.values.find(_.eq(str)) orElse
      CondOperator.values.find(_.eq(str)) orElse
      (str match
        case "," => Some(Coma)
        case _ => None
      ) getOrElse
      StrToken(str)
  }

  def parseSeq[T](f: PartialParser[T], until: Token)(tokens: Seq[Token]): Either[String, ParserResult[Seq[T]]] = {
    @tailrec
    def inner(rest: Seq[Token], acc: Seq[T]): Either[String, ParserResult[Seq[T]]] = {
      f.lift(rest) match
        case None => Left(s"Cannot parse sequence: ${rest.mkString(" ")}")
        case Some(ParserResult(result, Seq())) => Right(ParserResult(acc :+ result, Seq()))
        case Some(ParserResult(result, head +: tail)) if head == until =>
          Right(ParserResult(acc :+ result, tail))
        case Some(ParserResult(result, Coma +: tail)) => inner(tail, acc :+ result)
        case Some(ParserResult(_, head +: tail)) =>
          Left(s"Cannot parse sequence, bumped into ${head.toString}, rest: ${tail.mkString(" ")}")
    }

    inner(tokens, Seq.empty)
  }

  val literalParser: PartialParser[Literal] = {
    case StrToken(s"'$str'") +: tail => ParserResult(StringLiteral(str), tail)
    case StrToken(str) +: tail  if str.toIntOption.isDefined => ParserResult(IntLiteral(str.toInt), tail)
    case StrToken(str) +: tail if str.toBooleanOption.isDefined => ParserResult(BooleanLiteral(str.toBoolean), tail)
  }

  val columnRefParser: PartialParser[ColumnRef] = {
    case StrToken(s"$tableRef.$column") +: tail =>
      ParserResult(ColumnRef(column, Some(tableRef)), tail)
    case StrToken(column) +: tail =>
      ParserResult(ColumnRef(column, None), tail)
  }

  lazy val functionParser: PartialParser[Function] = {
    // TODO: include expressions
    val argParser: PartialParser[Literal | ColumnRef] = columnRefParser orElse literalParser

    ???
  }

  lazy val expressionParser: PartialParser[Expr] =
    literalParser

  lazy val aliasParser: PartialParser[Alias] = {
    val inner: PartialFunction[ParserResult[Expr | ColumnRef], ParserResult[Alias]] = {
      case ParserResult(e, Symbols.AS +: StrToken(alias) +: tail) => ParserResult(Alias(e, alias), tail)
    }

    (columnRefParser andThen inner) orElse (expressionParser andThen inner)
  }

//  val columnParser_ : PartialParser[Column] = (tokens: Seq[Token]) => {
//    def inner(column: Option[Column], tokens: Seq[Token]): Either[String, ParserResult[Column]] = (column, tokens) match {
//      case (None, Seq()) => Left("nothing to parse")
//      case (None, Literal(s"$tableRef.$column") +: tail) => inner(Some(Column(column, Some(tableRef), None)), tail)
//      case (None, Literal(column) +: tail) => inner(Some(Column(column, None, None)), tail)
//      case (None, head +: _) => Left(s"Tried to parse $head into column")
//      case (Some(v), Seq()) => Right(ParserResult(v, Seq.empty))
//      case (Some(v), (head@(Coma | _: Symbols | _: CondOperator)) +: tail) => ???
//      case (Some(v), Coma +: tail) => Right(ParserResult(v, tail))
//      case (Some(v), Symbols.AS +: Literal(alias) +: tail) => ???
//    }
//
//    inner(None, tokens)
//  }

  def parseUntilFrom(tokens: Seq[Token]): Either[String, (Seq[StrToken], Seq[Token])] = {
    @tailrec
    def inner(rest: Seq[Token], columns: Seq[StrToken]): Either[String, (Seq[StrToken], Seq[Token])] = rest match
      case Nil if columns.isEmpty => Left(s"There are no columns for SELECT")
      case Symbols.FROM +: Nil => Left(s"There are no columns for SELECT, next token FROM")
      case Symbols.FROM +: tail => Right((columns, tail))
      case (head: StrToken) +: StrToken(",") +: tail => inner(tail, columns :+ head)
      case (head: StrToken) +: tail => inner(tail, columns :+ head)
      case rest => Left(s"Got strange sequence of tokens when parsing select: ${rest.mkString(" ")}")

    inner(tokens, Seq.empty)
  }

  def parseFrom(tokens: Seq[Token]): Either[String, StrToken] = tokens match
    case (head: StrToken) +: _ => Right(head)
    case head +: _ => Left(s"Expected table name after FROM, got $head")

  def parseWhere(tokens: Seq[Token]): Either[String, Option[List[Where]]] = tokens match
    case head +: _ if !Symbols.WHERE.eq(head) => Right(None)
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

  val queryParserOld: PartialFunction[Seq[Token], Either[String, ParserResult[Query]]] = {
    case head +: tail if Symbols.SELECT.eq(head) =>
      for {
        result <- parseUntilFrom(tail)
        (columns, tail2) = result
        select = Select(columns)

        fromLiteral <- parseFrom(tail2)
        from = From(fromLiteral)

        tail3 = tail2.tail
        where <- parseWhere(tail3)

      } yield ParserResult(Query(select, from, where), ???)

  }

  def preprocess(s: String): Either[String, Seq[Token]] =
    filter(splitTokens(s))
      .flatMap(v => Right(splitComas(v)))
      .map(parseToTokens)

  def parse(s: String): Either[String, Query] =
    preprocess(s)
      .flatMap(queryParserOld orElse {
        case head +: _ => Left(s"Query starts with $head not with ${Symbols.SELECT}")
      })
      .map(_.result)
}


/*

  SELECT * FROM table WHERE 1=1 TAKE 10 SKIP 10

 */
