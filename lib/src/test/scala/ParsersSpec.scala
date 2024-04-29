import Ast.CondOperator.*
import Ast.Symbols.*
import Ast.{StrToken, StrToken as S, *}
import Parsers.*
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import QueryFixtures._
import Utils._

import scala.util.Right

class ParsersSpec extends AnyWordSpecLike with Matchers with EitherValues {

  "Parsers" should {
    "literal parser" in {
      val inOut = List(
        ("'321413'", Right(ParserResult(StringLiteral("321413"), Seq.empty))),
        ("321413", Right(ParserResult(IntLiteral(321413), Seq.empty))),
        ("true", Right(ParserResult(BooleanLiteral(true), Seq.empty))),
        ("false zhopa", Right(ParserResult(BooleanLiteral(false), Seq(S("zhopa"))))),
        ("FROM", Left(""))
      )

      checkParser(literalParser)(inOut)
    }

    "columnRefParser" in {
      val inOut = List(
        ("name", Right(ParserResult(ColumnRef("name", None), Seq.empty))),
        ("t.name", Right(ParserResult(ColumnRef("name", Some("t")), Seq.empty))),
        ("t.name from", Right(ParserResult(ColumnRef("name",  Some("t")), Seq(Symbols.From)))),
        ("FROM", Left(""))
      )


      checkParser(columnRefParser)(inOut)
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

      checkParser(functionParser)(inOut)
    }

    "expressionParser" in {
      val inOut: Seq[(String, ErrOr[ParserResult[Expr]])] = List(
        (
          "true",
          Right(ParserResult(BooleanLiteral(true), Seq.empty))
        ),
        (
          "concat( 'zhopa' , t.name )",
          Right(ParserResult(FunctionCall("concat", Seq(StringLiteral("zhopa"), ColumnRef("name", Some("t")))), Seq.empty))
        ),
        (s"(${simpleQuery._1})", simpleQuery._2),
        (
          "FROM", Left("")
        )
      )

      checkParser(expressionParser)(inOut)
    }

    "basicBoolExprParser" in {
      val inOut = List(
        (
          "1 = 1",
          Right(ParserResult(BasicBoolExpr(Equals, IntLiteral(1), IntLiteral(1)), Seq.empty))
        ),
        (
          "name like '%zhopa%'",
          Right(ParserResult(BasicBoolExpr(Like, ColumnRef("name", None), StringLiteral("%zhopa%")), Seq.empty))
        ),
        (
          "table1.isAdmin >= table1.isAJoke",
          Right(ParserResult(BasicBoolExpr(GtThan, ColumnRef("isAdmin", Some("table1")), ColumnRef("isAJoke", Some("table1"))), Seq.empty))
        ),
        ("FROM", Left(""))
      )

      checkParser(basicBoolExprParser)(inOut)
    }

    "betweenParser" in {
      val inOut = List(
        (
          "table1.a BETWEEN count(table1.b) AND 100",
          Right(ParserResult(BetweenExpr(ColumnRef("a", Some("table1")), FunctionCall("count", Seq(ColumnRef("b", Some("table1")))), IntLiteral(100)), Seq.empty))
        ),
        ("FROM", Left(""))
      )

      checkParser(betweenParser)(inOut)
    }

    "boolExprParser" in {
      val inOut = List(
        (
          "name LIKE '%zhopa%' AND table1.a BETWEEN count(table1.b) AND 100 OR t.count = 0",
          Right(ParserResult(ComplexBoolExpr(
            And,
            BasicBoolExpr(Like, ColumnRef("name", None), StringLiteral("%zhopa%")),
            ComplexBoolExpr(
              Or,
              BetweenExpr(ColumnRef("a", Some("table1")), FunctionCall("count", Seq(ColumnRef("b", Some("table1")))), IntLiteral(100)),
              BasicBoolExpr(Equals, ColumnRef("count", Some("t")), IntLiteral(0))
            )
          ), Seq.empty))
        ),
        (
          "(c1 = 1 and id < 0) or (c1 = 2 and id > 0)",
          Right(ParserResult(ComplexBoolExpr(
            Or,
            ComplexBoolExpr(
              And,
              BasicBoolExpr(Equals, ColumnRef("c1", None), IntLiteral(1)),
              BasicBoolExpr(Less, ColumnRef("id", None), IntLiteral(0))
            ),
            ComplexBoolExpr(
              And,
              BasicBoolExpr(Equals, ColumnRef("c1", None), IntLiteral(2)),
              BasicBoolExpr(Gt, ColumnRef("id", None), IntLiteral(0))
            )
          ), Seq.empty))
        ),
        ("id = 1",
          Right(ParserResult(
            BasicBoolExpr(Equals, ColumnRef("id", None), IntLiteral(1))
          , Seq.empty))
        ),
        ("FROM", Left(""))
      )

      checkParser(boolExprParser)(inOut)
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
          "1 as a",
          Right(ParserResult(Alias(IntLiteral(1), "a"), Seq.empty))
        ),
        (
          "concat( 'zhopa' , t.name ) as b",
          Right(ParserResult(Alias(FunctionCall("concat", Seq(StringLiteral("zhopa"), ColumnRef("name", Some("t")))), "b"), Seq.empty))
        ),
        (
          s"(${simpleQuery._1}) as a",
          Right(ParserResult(Alias(simpleQuery._2.value.result, "a"), Seq.empty))
        ),
        (
          "true",
          Left("")
        )
      )

      checkParser(aliasParser)(inOut)
    }

    "tableAliasParser" in {
      val inOut = List(
        (
          "users as u",
          Right(ParserResult(TableAlias(S("users"), "u"), Seq.empty))
        ),
        (
          "count(1) as t1",
          Right(ParserResult(TableAlias(FunctionCall("count", Seq(IntLiteral(1))), "t1"), Seq.empty))
        ),
        (
          "(select name from t1) as t3",
          Right(ParserResult(TableAlias(Query(Seq(ColumnRef("name", None)), StrToken("t1"), None), "t3"), Seq.empty))
        ),
        (
          "true",
          Left("")
        )
      )

      checkParser(tableAliasParser)(inOut)
    }

    "queryParser" in {
      val inOut = List(
        (
          "select name from table WHERE zhopa > 10 OR table.isTrue AND table.zhepa = 0",
          Right(ParserResult(Query(
            List(ColumnRef("name", None)),
            S("table"),
            Some(ComplexBoolExpr(
              Or,
              BasicBoolExpr(Gt, ColumnRef("zhopa", None), IntLiteral(10)),
              ComplexBoolExpr(
                And,
                ColumnRef("isTrue", Some("table")),
                BasicBoolExpr(
                  Equals,
                  ColumnRef("zhepa", Some("table")),
                  IntLiteral(0)
                )
              )
            ))
          ), Seq.empty))
        ),
        simpleQuery,
        complicatedQuery,
        (
          "true",
          Left("")
        )
      )

      checkParser(queryParser)(inOut)
    }
  }
}
