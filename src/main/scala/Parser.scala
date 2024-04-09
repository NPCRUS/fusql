import Ast.Token

case class ParserResult[+T](result: T, tail: Seq[Token])

sealed trait Parser[M[_], T] {
  import Parser._

  def apply(input: Seq[Token]): M[ParserResult[T]]
}

type ErrOr[T] = Either[String, T]

object Parser {

  case class PartialParser[T](f: PartialFunction[Seq[Token], ParserResult[T]]) extends Parser[Option, T] {
    override def apply(input: Seq[Token]): Option[ParserResult[T]] = f.lift(input)

    def orElse[B](that: PartialParser[B]): PartialParser[T | B] = PartialParser(f orElse that.f)
    
    def full(parserName: String = "unknown"): FullParser[T] = FullParser(
      f.lift andThen {
        case None => Left(s"Cannot parse $parserName")
        case Some(value) => Right(value)
      }
    )
  }

  case class FullParser[T](f: Seq[Token] => ErrOr[ParserResult[T]]) extends Parser[ErrOr, T] {
    override def apply(input: Seq[Token]): ErrOr[ParserResult[T]] = f(input)
    
    def orElse[B](that: FullParser[B]): FullParser[T | B] =
      FullParser { seq =>
        f(seq) match
          case Left(value) => that.f(seq)
          case Right(value) => Right(value)
      }

    def flatMap[B](f: ParserResult[T] => ErrOr[ParserResult[B]]): FullParser[B] = FullParser { seq =>
      this.f(seq).flatMap(f)
    }
    
    def map[B](f: T => B): FullParser[B] = FullParser { seq =>
      this.f(seq).map(v => v.copy(result = f(v.result)))
    }
  }

  def partial[T](f: PartialFunction[Seq[Token], ParserResult[T]]): PartialParser[T] = PartialParser(f)

  def full[T](f: Seq[Token] => ErrOr[ParserResult[T]]): FullParser[T] = FullParser(f)

}
