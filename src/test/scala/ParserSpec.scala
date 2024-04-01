import Ast.{Column, Literal, Symbols}
import Parser.{ParserResult, columnParser, preprocess}
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class ParserSpec extends AnyWordSpecLike with Matchers with EitherValues {

  "Parser" should {
    "columnParser checks" in {
      val inOut = List(
        ("name", ParserResult(Column("name", None, None), Seq.empty)),
        ("name as n1", ParserResult(Column("name", None, Some("n1")), Seq.empty)),
        ("t.name", ParserResult(Column("name", Some("t"), None), Seq.empty)),
        ("t.name as n1", ParserResult(Column("name",  Some("t"), Some("n1")), Seq.empty)),
        ("t.name as n1 from", ParserResult(Column("name",  Some("t"), Some("n1")), Seq(Symbols.FROM))),
      )


      inOut.foreach { (in, expectation) =>
        preprocess(in).map(columnParser).value shouldBe expectation
      }
    }
  }
}
