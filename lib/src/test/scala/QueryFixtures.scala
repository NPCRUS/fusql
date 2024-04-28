import Ast.*
import Ast.CondOperator.{And, Equals, Gt, Less, Or}
import org.scalatest.EitherValues

object QueryFixtures extends EitherValues {
  val simpleQuery: (String, Either[String, ParserResult[Query]]) = (
    "SELECT c1 FROM t1 WHERE 1 = 1",
    Right(ParserResult(Query(
      Seq(ColumnRef("c1", None)), 
      StrToken("t1"), 
      Some(BasicBoolExpr(Equals, IntLiteral(1), IntLiteral(1)))
    ), Seq.empty))
  )
  
  val complicatedQuery: (String, Either[String, ParserResult[Query]]) = (
    s"SELECT 1 as c1,(${simpleQuery._1}) as count, t1.name, id FROM (${simpleQuery._1}) as t1 WHERE (c1 = 1 and id < 0) or (c1 = 2 and id > 0)",
    Right(ParserResult(Query(
      Seq(
        Alias(IntLiteral(1), "c1"),
        Alias(
          simpleQuery._2.value.result,
          "count"
        ),
        ColumnRef("name", Some("t1")),
        ColumnRef("id", None)
      ),
      TableAlias(
        simpleQuery._2.value.result,
        "t1"
      ),
      Some(
        ComplexBoolExpr(
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
        )
      )
    ), Seq.empty))
  )
}
