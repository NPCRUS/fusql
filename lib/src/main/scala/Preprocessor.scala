import Ast.*
import Ast.Symbols.*
import Ast.CondOperator.*

object Preprocessor {
  private val forbiddenSymbols = Set("insert", "update" , "delete", "truncate", "drop")

  // TODO: add \n support
  def apply(str: String): Either[String, Seq[Token]] =
    filter(splitTokensByEmptySpace(str))
      .map(splitBy(Seq(Coma, BlockOpen, BlockClose, LessThan, GtThan, NotEquals, Equals, Gt, Less))(_))
      .map(parseToTokens)
  
  def parseToTokens(tokens: Seq[String | Token]): Seq[Token] = tokens.map {
    case str: String =>
      Symbols.values.find(_.eq(str)) orElse
        CondOperator.values.find(_.eq(str)) getOrElse
        StrToken(str)
    case other: Token => other
  }

  def splitTokensByEmptySpace(s: String): Seq[String] = s.split(" ")

  def filter(tokens: Seq[String]): Either[String, Seq[String]] = forbiddenSymbols.filter(tokens.contains).toList match
    case Nil => Right(tokens)
    case some => Left(s"Bumped into some forbidden keywords: ${some.mkString(",")}")
  
  def splitBy(separators: Seq[Token])(input: Seq[String]): Seq[String | Token] =
    separators.foldLeft(input.asInstanceOf[Seq[String | Token]]) { (acc, separator) =>
      acc.flatMap {
        case str: String if str == separator.repr =>
          Seq(separator)
        case str: String if str.contains(separator.repr) =>
          val split = if (separator.repr.length > 1) str.split(separator.repr) else str.split(separator.repr(0))
          split.toSeq match
            case "" +: Seq() =>
              Seq(separator)
            case head +: Seq() =>
              Seq(head, separator)
            case seq =>
              seq.flatMap {
                case "" => Seq(separator)
                case str => Seq(str, separator)
              }.dropRight(1)
        case rest => Seq(rest)
      }
    }

  def separateToken(token: String)(input: String): Seq[String] = input match {
    case v if v.contains(token) =>
      val separator = if(token.length > 1) v.split(token) else v.split(token(0))
      separator.toSeq match
        case "" +: Seq() =>
          Seq(token)
        case head +: Seq() =>
          Seq(head, token)
        case seq =>
          seq.flatMap {
            case "" => Seq(token)
            case str => Seq(str, token)
          }.dropRight(1)
    case rest => Seq(rest)
  }
}
