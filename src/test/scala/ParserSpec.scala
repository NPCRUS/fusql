import Ast.{BooleanLiteral, ColumnRef, IntLiteral, StrToken, StringLiteral, Symbols}
import Parser.{ParserResult, Parser, columnRefParser, literalParser, preprocess}
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class ParserSpec extends AnyWordSpecLike with Matchers with EitherValues {

  "Parser" should {
    "PartialParserWithErrorTest" in {
      val original: Parser[Int] = {
        case StrToken(str) +: tail if str.toIntOption.isDefined => Right(ParserResult(str.toInt, tail))
        case StrToken("hello") +: tail => Left("wtf")
      }

      val next: PartialFunction[ParserResult[Int], Either[String, ParserResult[Int]]] = {
        case ParserResult(v, tail) if v > 5 => Right(ParserResult(v, tail))
      }

      val parser = original.flatMap(next)

      val parser2 = parser.flatMap {
        case ParserResult(result, tail) if result > 15 => Left("wtf")
      }

      parser.lift(Seq(StrToken("3"))) shouldBe None
      parser.lift(Seq(StrToken("6"))) shouldBe Some(Right(ParserResult(6, Seq.empty)))
      parser.lift(Seq(StrToken("hello"))) shouldBe Some(Left("wtf"))
      parser2.lift(Seq(StrToken("16"))) shouldBe Some(Left("wtf"))
    }

    "literal parser" in {
      val inOut = List(
        ("'321413'", ParserResult(StringLiteral("321413"), Seq.empty)),
        ("321413", ParserResult(IntLiteral(321413), Seq.empty)),
        ("true", ParserResult(BooleanLiteral(true), Seq.empty)),
        ("false zhopa", ParserResult(BooleanLiteral(false), Seq(StrToken("zhopa")))),
      )

      inOut.foreach { (in, expectation) =>
        preprocess(in).map(literalParser).value shouldBe expectation
      }
    }

    "columnParser" in {
      val inOut = List(
        ("name", ParserResult(ColumnRef("name", None), Seq.empty)),
        ("t.name", ParserResult(ColumnRef("name", Some("t")), Seq.empty)),
        ("t.name from", ParserResult(ColumnRef("name",  Some("t")), Seq(Symbols.FROM))),
      )


      inOut.foreach { (in, expectation) =>
        preprocess(in).map(columnRefParser).value shouldBe expectation
      }
    }
  }
}
