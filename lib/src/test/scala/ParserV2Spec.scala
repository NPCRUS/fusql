import org.scalatest.wordspec.AnyWordSpecLike
import fastparse.*, SingleLineWhitespace.*
import astV2.*
import astV2.Literal.*
import fastparse.Parsed.Failure
import org.scalatest.matchers.should.Matchers
import parserV2.*
import ComparisonOperator.*

import java.time.LocalDateTime

class ParserV2Spec extends AnyWordSpecLike with Matchers {

  def run[A](inOut: Seq[(String, A)], parser: P[_] => P[A]): Unit =
    inOut.foreach { (in, out) =>
      println(in)
      val result = parse(in, parser(_), verboseFailures = true)
      if(result.isSuccess) {
        result.get.value shouldBe out
      } else {
        val failure @ Parsed.Failure(input, _, extra) = result
        println(failure.longMsg)
        assert(false)
      }
    }

  "literalP" in {
    val inOut = Seq(
      ("true", Bool(true)),
      ("false", Bool(false)),
      ("1", Int(1)),
      ("'str'", Str("str")),
      ("'2010-01-02 03:04:05'", Timestamp(LocalDateTime.parse("2010-01-02T03:04:05"))),
    )

    run(inOut, literalP(_))
  }

  "columnRefP" in {
    val inOut = Seq(
      ("c1", ColumnName("c1")),
      ("t1.c1", Column(TableName("t1"), ColumnName("c1"))),
      ("s1.t1.c1", Column(Schema("s1", TableName("t1")), ColumnName("c1")))
    )

    run(inOut, columnRefP(_))
  }

  "aggFunctionP" in {
    val inOut = Seq(
      ("count(c1,2)", AggFunction("count", Seq(ColumnName("c1"), Int(2))))
    )

    run(inOut, aggFunctionP(_))
  }

  // TODO: add query
  "selectAliasP" in {
    val inOut = Seq(
      (
        "t1.c1 as c2",
        SelectAlias(Column(TableName("t1"), ColumnName("c1")), "c2")
      ),
      (
        "count(true) as c2",
        SelectAlias(AggFunction("count", Seq(Bool(true))), "c2")
      )
    )

    run(inOut, selectAliasP(_))
  }

  // TODO: add query
  "tableRefP" in {
    val inOut = Seq(
      ("t1", TableName("t1")),
      ("s1.t1", Schema("s1", TableName("t1"))),
    )

    run(inOut, tableRefP(_))
  }

  "comparisonOperatorP" in {
    val inOut = Seq(
      ("c1=c2", Eq(ColumnName("c1"), ColumnName("c2"))),
      (
        "t1.c1 <> c2",
        NotEq(Column(TableName("t1"), ColumnName("c1")), ColumnName("c2"))
      ),
      (
        "s1.t1.c1 > t1.c2",
        Gt(
          Column(Schema("s1", TableName("t1")), ColumnName("c1")),
          Column(TableName("t1"), ColumnName("c2"))
        )
      ),
      ("c1<10", Lt(ColumnName("c1"), Int(10))),
      ("c1>=true", Lt(ColumnName("c1"), Bool(true)))
    )

    run(inOut, comparisonOperatorP(_))
  }
}
