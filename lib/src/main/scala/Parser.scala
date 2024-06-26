import Ast.Token
import Ast.Symbols.*

import scala.annotation.tailrec

// TODO: https://com-lihaoyi.github.io/fastparse/#ExampleParsers
// Omnipresent Li Haoyi made library for this as well
case class ParserResult[+T](result: T, tail: Seq[Token])

type ErrOr[T] = Either[String, T]

sealed trait Parser[T] {
  import Parser.ParserImpl
  
  def apply(input: Seq[Token]): ErrOr[ParserResult[T]]
  
  def name: String
  
  def withName(name: String): Parser[T] = ParserImpl(s"$name(${this.name})")(this.apply)
  
  private def replaceName(name: String): Parser[T] = ParserImpl(name)(this.apply)

  def flatMap[B](f: ParserResult[T] => ErrOr[ParserResult[B]]): Parser[B] = ParserImpl(name) { seq =>
    apply(seq).flatMap(f)
  }

  def map[B](f: T => B): Parser[B] = ParserImpl(name) { seq =>
    apply(seq).map(v => v.copy(result = f(v.result)))
  }
  
  def orElse[B](that: Parser[B]): Parser[T | B] =
    ParserImpl(s"$name or ${that.name}") { seq =>
      apply(seq) match
        case Left(value) =>
          that.apply(seq)
        case Right(value) => Right(value)
    }

  def andThen[B](that: Parser[B]): Parser[(T, B)] =
    this.flatMap {
      case ParserResult(t, rest) =>
        that.apply(rest).map { result =>
          result.copy(result = (t, result.result))
        }
    }.replaceName(s"${this.name} and ${that.name}")
    
  def option: Parser[Option[T]] = ParserImpl(s"option($name)") { seq =>
    this.apply(seq) match
      case Left(value) => Right(ParserResult(None, seq))
      case Right(value) => Right(value.copy(result = Some(value.result)))
  }

  def orEnclosed: Parser[T] = ParserImpl(s"orEnclosed($name)") {
    case seq@(BlockOpen +: tail) => this.andThen(Parser.token(BlockClose)).apply(tail).map(res => res.copy(result = res.result._1))
    case seq => this.apply(seq)
  }
  
  def enclosed: Parser[T] = ParserImpl(s"enclosed($name)") {
    case BlockOpen +: tail => apply(tail).flatMap {
      case ParserResult(result, BlockClose +: tail) => Right(ParserResult(result, tail))
      case ParserResult(result, _) => Left(s"Didn't find ')' when trying to parse block of name")
    }
    case seq => Left(s"Didn't find '(' when trying to parse block of name")
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
      case None => Left(s"Cannot parse $name at ${seq.mkString(" ")}")
  }
  
  def partial[T](f: PartialFunction[Seq[Token], ParserResult[T]]): Parser[T] = partial("")(f)
    
  def option[T, B](optionalParser: Parser[T])(seq: Seq[Token])(f: T => Parser[B]): ErrOr[ParserResult[Option[B]]] = {
    optionalParser.apply(seq) match
      case Left(value) => Right(ParserResult(None, seq))
      case Right(result) => f(result.result).apply(result.tail).map(finalResult => finalResult.copy(result = Some(finalResult.result)))
  }

  def token[T <: Token](t: Token): Parser[T] = partial(t.getClass.getSimpleName) {
    case (found: Token) +: tail if t.repr == found.repr => ParserResult(t.asInstanceOf[T], tail)
  }

  def seq[T](p: Parser[T], until: Token): Parser[Seq[T]] = {
    @tailrec
    def inner(rest: Seq[Token], acc: Seq[T]): Either[String, ParserResult[Seq[T]]] = {
      p.apply(rest) match
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

    Parser(s"seq(${p.name}") { tokens =>
      inner(tokens, Seq.empty)
    }
  }

}
