import Ast.Symbols.*
import Ast.{StrToken as S, *}
import Parser._
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class ParserSpec extends AnyWordSpecLike with Matchers with EitherValues {

  "Parser" should {
    "PartialParserWithError " in {
      val original: Parser[Int] = {
        case S(str) +: tail if str.toIntOption.isDefined => Right(ParserResult(str.toInt, tail))
        case S("hello") +: tail => Left("wtf")
      }

      val next: PartialFunction[ParserResult[Int], Either[String, ParserResult[Int]]] = {
        case ParserResult(v, tail) if v > 5 => Right(ParserResult(v, tail))
      }

      val parser = original.flatMap(next)

      val parser2 = parser.flatMap {
        case ParserResult(result, tail) if result > 15 => Left("wtf")
      }

      parser.lift(Seq(S("3"))) shouldBe None
      parser.lift(Seq(S("6"))) shouldBe Some(Right(ParserResult(6, Seq.empty)))
      parser.lift(Seq(S("hello"))) shouldBe Some(Left("wtf"))
      parser2.lift(Seq(S("16"))) shouldBe Some(Left("wtf"))
    }

    "enrichWithToken" in {
      val result = Seq(S("test(bla,bla,bla)zhopa"))
        .flatMap(enrichWithToken(Coma))
        .flatMap(enrichWithToken(BlockOpen))
        .flatMap(enrichWithToken(BlockClose))

      result shouldBe Seq(S("test"), BlockOpen, S("bla"), Coma, S("bla"), Coma, S("bla"), BlockClose, S("zhopa"))

    }

    "literal parser" in {
      val inOut = List(
        ("'321413'", ParserResult(StringLiteral("321413"), Seq.empty)),
        ("321413", ParserResult(IntLiteral(321413), Seq.empty)),
        ("true", ParserResult(BooleanLiteral(true), Seq.empty)),
        ("false zhopa", ParserResult(BooleanLiteral(false), Seq(S("zhopa")))),
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

    "functionParser" in {
      val inOut = List(
        (
          "count(t.name)",
          Right(ParserResult(FunctionCall("count", Seq(ColumnRef("name", Some("t")))), Seq.empty))
        ),
        (
          "concat( 'zhopa', t.name )",
          Right(ParserResult(FunctionCall("concat", Seq(StringLiteral("zhopa"), ColumnRef("name", Some("t")))), Seq.empty))
        ),
        ("arr_aggr([])", Left("")),
        ("concat( 'zhopa' t.name )", Left(""))
      )

      inOut.foreach { (in, expectation) =>
        expectation match {
          case Right(value) =>
            preprocess(in).flatMap(functionParser).value shouldBe value
          case Left(_) =>
            val errorResult = preprocess(in).flatMap(functionParser)
            println(errorResult.left.value)
            errorResult.isLeft shouldBe true
        }

      }
    }
  }
}
