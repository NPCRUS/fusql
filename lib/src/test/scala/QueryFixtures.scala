import Ast.*
import Ast.CondOperator.Equals

object QueryFixtures {
  val simpleQuery: (String, Either[String, ParserResult[Query]]) = (
    "SELECT c1 FROM t1 WHERE 1 = 1",
    Right(ParserResult(Query(
      Seq(ColumnRef("c1", None)), 
      StrToken("t1"), 
      Some(BasicBoolExpr(Equals, IntLiteral(1), IntLiteral(1)))
    ), Seq.empty))
  )
}
