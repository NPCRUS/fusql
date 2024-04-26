import Ast.*
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class ParserSpec extends AnyWordSpecLike with Matchers with EitherValues {

  private val specificParser = Parser.partial("specificParser") {
    case StrToken("*") +: tail => ParserResult("good", tail)
  }

  "enclosed" in {
    val inOut = List(
      ("*", Left("")),
      ("(*", Left("")),
      ("(*)", Right(ParserResult("good", Seq.empty)))
    )

    inOut.foreach { (in, expectation) =>
      val result = Preprocessor(in).flatMap(specificParser.enclosed.apply)
      expectation match {
        case Right(value) =>
          result.value shouldBe value
        case Left(_) =>
          result.isLeft shouldBe true
      }
    }
  }

  "orEnclosed" in {
    val inOut = List(
      ("*", Right(ParserResult("good", Seq.empty))),
      ("(*", Left("")),
      ("(*)", Right(ParserResult("good", Seq.empty)))
    )

    inOut.foreach { (in, expectation) =>
      val result = Preprocessor(in).flatMap(specificParser.orEnclosed.apply)
      expectation match {
        case Right(value) =>
          result.value shouldBe value
        case Left(_) =>
          result.isLeft shouldBe true
      }
    }
  }
}
