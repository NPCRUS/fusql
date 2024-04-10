import Ast.Symbols.*
import Ast.{StrToken as S, *}
import Parsers.*
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.util.Right

class ParsersSpec extends AnyWordSpecLike with Matchers with EitherValues {

  "Parser" should {
    "enrichWithToken" in {
      val result = Seq(S("test(bla,bla,bla)zhopa"))
        .flatMap(enrichWithToken(Coma))
        .flatMap(enrichWithToken(BlockOpen))
        .flatMap(enrichWithToken(BlockClose))

      result shouldBe Seq(S("test"), BlockOpen, S("bla"), Coma, S("bla"), Coma, S("bla"), BlockClose, S("zhopa"))

    }

    "literal parser" in {
      val inOut = List(
        ("'321413'", Some(ParserResult(StringLiteral("321413"), Seq.empty))),
        ("321413", Some(ParserResult(IntLiteral(321413), Seq.empty))),
        ("true", Some(ParserResult(BooleanLiteral(true), Seq.empty))),
        ("false zhopa", Some(ParserResult(BooleanLiteral(false), Seq(S("zhopa"))))),
        ("FROM", None)
      )

      inOut.foreach { (in, expectation) =>
        preprocess(in).map(literalParser.apply).value shouldBe expectation
      }
    }

    "parseSeq" in {
      val inOut = List(
        ("'321413'", Right(ParserResult(List(StringLiteral("321413")), Seq.empty))),
      )

      inOut.foreach { (in, expectation) =>
        preprocess(in).flatMap(parseSeq(literalParser.full(""), FROM).apply) shouldBe expectation
      }
    }

    "columnParser" in {
      val inOut = List(
        ("name", Some(ParserResult(ColumnRef("name", None), Seq.empty))),
        ("t.name", Some(ParserResult(ColumnRef("name", Some("t")), Seq.empty))),
        ("t.name from", Some(ParserResult(ColumnRef("name",  Some("t")), Seq(Symbols.FROM)))),
      )


      inOut.foreach { (in, expectation) =>
        preprocess(in).map(columnRefParser.apply).value shouldBe expectation
      }
    }

    "functionParser" in {
      val inOut = List(
        (
          "count(t.name)",
          Right(ParserResult(FunctionCall("count", Seq(ColumnRef("name", Some("t")))), Seq.empty))
        ),
        (
          "concat( 'zhopa' , t.name )",
          Right(ParserResult(FunctionCall("concat", Seq(StringLiteral("zhopa"), ColumnRef("name", Some("t")))), Seq.empty))
        ),
        ("arr_aggr([])", Left("")),
        ("concat( 'zhopa' t.name )", Left(""))
      )

      inOut.foreach { (in, expectation) =>
        expectation match {
          case Right(value) =>
            preprocess(in).flatMap(functionParser.apply).value shouldBe value
          case Left(_) =>
            val errorResult = preprocess(in).flatMap(functionParser.apply)
            println(errorResult.left.value)
            errorResult.isLeft shouldBe true
        }

      }
    }

    "expressionParser" in {
      val inOut = List(
        (
          "true",
          Right(ParserResult(BooleanLiteral(true), Seq.empty))
        ),
        (
          "concat( 'zhopa' , t.name )",
          Right(ParserResult(FunctionCall("concat", Seq(StringLiteral("zhopa"), ColumnRef("name", Some("t")))), Seq.empty))
        ),
        (
          "FROM", Left("")
        )
      )

      inOut.foreach { (in, expectation) =>
        expectation match {
          case Right(value) =>
            preprocess(in).flatMap(expressionParser.apply).value shouldBe value
          case Left(_) =>
            val errorResult = preprocess(in).flatMap(expressionParser.apply)
            println(errorResult.left.value)
            errorResult.isLeft shouldBe true
        }

      }
    }

    "booleanExprParser" in {
      val inOut = List(
        (
          "1 = 1",
          Right(ParserResult(BooleanExprImpl(CondOperator.Equals, IntLiteral(1), IntLiteral(1)), Seq.empty))
        ),
        (
          "name like '%zhopa%'",
          Right(ParserResult(BooleanExprImpl(CondOperator.Like, ColumnRef("name", None), StringLiteral("%zhopa%")), Seq.empty))
        ),
        (
          "table1.isAdmin OR table1.isAJoke",
          Right(ParserResult(BooleanExprImpl(CondOperator.Or, ColumnRef("isAdmin", Some("table1")), ColumnRef("isAJoke", Some("table1"))), Seq.empty))
        ),
        ("FROM", Left(""))
      )

      inOut.foreach { (in, expectation) =>
        expectation match {
          case Right(value) =>
            preprocess(in).flatMap(booleanExprParser.apply).value shouldBe value
          case Left(_) =>
            val errorResult = preprocess(in).flatMap(booleanExprParser.apply)
            println(errorResult.left.value)
            errorResult.isLeft shouldBe true
        }

      }
    }

    "aliasParser" in {
      val inOut = List(
        (
          "t1.name as not_name",
          Right(ParserResult(Alias(ColumnRef("name", Some("t1")), "not_name"), Seq.empty))
        ),
        (
          "true as a",
          Right(ParserResult(Alias(BooleanLiteral(true), "a"), Seq.empty))
        ),
        (
          "concat( 'zhopa' , t.name ) as b",
          Right(ParserResult(Alias(FunctionCall("concat", Seq(StringLiteral("zhopa"), ColumnRef("name", Some("t")))), "b"), Seq.empty))
        ),
        (
          "true",
          Left("")
        )
      )

      inOut.foreach { (in, expectation) =>
        expectation match {
          case Right(value) =>
            preprocess(in).flatMap(aliasParser.apply).value shouldBe value
          case Left(_) =>
            val errorResult = preprocess(in).flatMap(aliasParser.apply)
            println(errorResult.left.value)
            errorResult.isLeft shouldBe true
        }

      }
    }

    "queryParser" in {
      val inOut = List(
        (
          "select name from table",
          Right(ParserResult(Query(List(ColumnRef("name", None)), S("table")), Seq.empty))
        ),
        (
          "true",
          Left("")
        )
      )

      inOut.foreach { (in, expectation) =>
        expectation match {
          case Right(value) =>
            preprocess(in).flatMap(queryParser.apply).value shouldBe value
          case Left(_) =>
            val errorResult = preprocess(in).flatMap(queryParser.apply)
            println(errorResult.left.value)
            errorResult.isLeft shouldBe true
        }

      }
    }
  }
}
