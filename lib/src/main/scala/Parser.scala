import Ast.Token
import Ast.Symbols._

// TODO: https://com-lihaoyi.github.io/fastparse/#ExampleParsers
// Omnipresent Li Haoyi made library for this as well
case class ParserResult[+T](result: T, tail: Seq[Token])

type ErrOr[T] = Either[String, T]

sealed trait Parser[T] {
  import Parser.ParserImpl
  
  def apply(input: Seq[Token]): ErrOr[ParserResult[T]]
  
  def name: String
  
  def orElse[B](that: Parser[B]): Parser[T | B] =
    ParserImpl(s"$name or ${that.name}") { seq =>
      apply(seq) match
        case Left(value) =>
          that.apply(seq)
        case Right(value) => Right(value)
    }
    
  def orEnclosed: Parser[T] = this.enclosed.orElse(this)
  
  def enclosed: Parser[T] = ParserImpl(name) {
    case BlockOpen +: tail => apply(tail).flatMap {
      case ParserResult(result, BlockClose +: tail) => Right(ParserResult(result, tail))
      case ParserResult(result, _) => Left(s"Didn't find ')' when trying to parse block of name")
    }
    case seq => Left(s"Didn't find '(' when trying to parse block of name")
  }

  def flatMap[B](f: ParserResult[T] => ErrOr[ParserResult[B]]): Parser[B] = ParserImpl(name) { seq =>
    apply(seq).flatMap(f)
  }

  def map[B](f: T => B): Parser[B] = ParserImpl(name) { seq =>
    apply(seq).map(v => v.copy(result = f(v.result)))
  }
}

object Parser {
  
  case class ParserImpl[T](name: String)(f: Seq[Token] => ErrOr[ParserResult[T]]) extends Parser[T] {
    override def apply(input: Seq[Token]): ErrOr[ParserResult[T]] = f(input)
  }
  
  def apply[T](name: String)(f: Seq[Token] => ErrOr[ParserResult[T]]): Parser[T] = ParserImpl(name)(f)

  def apply[T](f: Seq[Token] => ErrOr[ParserResult[T]]): Parser[T] = ParserImpl("")(f)
  
  def partial[T](name: String)(f: PartialFunction[Seq[Token], ParserResult[T]]): Parser[T] = ParserImpl(name) { seq =>
    f.lift(seq) match
      case Some(value) => Right(value)
      case None => Left(s"Cannot parse with $name")
  }
  
  def partial[T](f: PartialFunction[Seq[Token], ParserResult[T]]): Parser[T] = partial("")(f)

}
