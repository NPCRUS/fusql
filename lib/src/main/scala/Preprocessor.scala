import Ast.*
import Ast.Symbols.*
import Ast.CondOperator.*

object Preprocessor {
  private val forbiddenSymbols = Set("insert", "update" , "delete", "truncate", "drop")

  // TODO: filter for strange symbols, e.g. ==, <-, ->
  def apply(str: String): Either[String, Seq[Token]] =
    filter(splitTokensByEmptySpace(str))
      .map(parseToTokens)
      .map { seq =>
        seq
          .flatMap(separateSymbol(Coma))
          .flatMap(separateSymbol(BlockOpen))
          .flatMap(separateSymbol(BlockClose))
          .flatMap(separateSymbol(LessThan))
          .flatMap(separateSymbol(GtThan))
          .flatMap(separateSymbol(NotEquals))
          .flatMap(separateSymbol(Equals))
          .flatMap(separateSymbol(Gt))
          .flatMap(separateSymbol(Less))
      }

  def parseToTokens(tokens: Seq[String]): Seq[Token] = tokens.map { str =>
    Symbols.values.find(_.eq(str)) orElse
      CondOperator.values.find(_.eq(str)) getOrElse
      StrToken(str)
  }
  
  def splitTokensByEmptySpace(s: String): Seq[String] = s.split(" ")

  def filter(tokens: Seq[String]): Either[String, Seq[String]] = forbiddenSymbols.filter(tokens.contains).toList match
    case Nil => Right(tokens)
    case some => Left(s"Bumped into some forbidden keywords: ${some.mkString(",")}")

  def separateSymbol(token: Token)(input: Token): Seq[Token] = input match {
    case StrToken(v) if v.contains(token.repr) =>
      v.split(token.repr.toCharArray).toSeq match
        case "" +: Seq() => Seq(token)
        case head +: Seq() => Seq(StrToken(head), token)
        case seq =>
          seq.flatMap {
            case "" => Seq(token)
            case str => Seq(StrToken(str), token)
          }.dropRight(1)
    case rest => Seq(rest)
  }
}
