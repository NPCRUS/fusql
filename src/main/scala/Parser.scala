import scala.annotation.tailrec
import Ast._

object Parser {

  private val forbiddenSymbols = Set("insert", "update" , "delete", "truncate")

  private def splitTokens(s: String): Seq[String] = s.split(" ")

  private def filter(tokens: Seq[String]): Either[String, Seq[String]] = forbiddenSymbols.filter(tokens.contains).toList match
    case Nil => Right(tokens)
    case some => Left(s"Bumped into some forbidden keywords: ${some.mkString(",")}")

  private def parseToTokens(tokens: Seq[String]): Seq[Token] = tokens.map { str =>
    Symbols.values.find(_.eq(str)) orElse
      CondOperator.values.find(_.eq(str)) orElse
      ComparisonOperator.values.find(_.eq(str)) getOrElse
      Literal.clean(str)
  }

  private def parseUntilFrom(tokens: Seq[Token]): Either[String, (Seq[Literal], Seq[Token])] = {
    @tailrec
    def inner(rest: Seq[Token], columns: Seq[Literal]): Either[String, (Seq[Literal], Seq[Token])] = rest match
      case Nil if columns.isEmpty => Left(s"There are no columns for SELECT")
      case Symbols.FROM +: Nil => Left(s"There are no columns for SELECT, next token FROM")
      case Symbols.FROM +: tail => Right((columns, tail))
      case (head: Literal) +: Literal(",") +: tail => inner(tail, columns :+ head)
      case (head: Literal) +: tail => inner(tail, columns :+ head)
      case rest => Left(s"Got strange sequence of tokens when parsing select: ${rest.mkString(" ")}")

    inner(tokens, Seq.empty)
  }
  
  private def parseFrom(tokens: Seq[Token]): Either[String, Literal] = tokens match
    case (head: Literal) +: _ => Right(head)
    case head +: _ => Left(s"Expected table name after FROM, got $head")

  private def parseWhere(tokens: Seq[Token]): Either[String, Option[List[Where]]] = tokens match
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

  private def parseCond(tokens: Seq[Token]): Either[String, (Cond, Seq[Token])] =
    tokens match
      case (left: Literal) +: (c: ComparisonOperator) +: (right: Literal) +: rest if c != ComparisonOperator.Between =>
        Right(SimpleCond(c, left, right), rest)
      case (left: Literal) +: ComparisonOperator.Between +: (leftBetween: Literal) +: CondOperator.And +: (rightBetween: Literal) +: rest =>
        Right(Between(left, leftBetween, rightBetween), rest)
      case seq =>
        Left(s"Got strange sequence of tokens when trying to parse cond: ${seq.mkString(" ")}")

  private def parseQuery(tokens: Seq[Token]): Either[String, Query] = tokens match
    case head +: tail if Symbols.SELECT.eq(head) =>
      for {
        result <- parseUntilFrom(tail)
        (columns, tail2) = result
        select = Select(columns)
        
        fromLiteral <- parseFrom(tail2)
        from = From(fromLiteral)

        tail3 = tail2.tail
        where <- parseWhere(tail3)

      } yield Query(select, from, where)
    case head +: _ => Left(s"Query starts with $head not with ${Symbols.SELECT}")


  def parse(s: String): Either[String, Query] =
    filter(splitTokens(s))
      .map(parseToTokens)
      .flatMap(parseQuery)
}


/*

  SELECT * FROM table WHERE 1=1 TAKE 10 SKIP 10

 */
