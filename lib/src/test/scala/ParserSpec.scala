import Ast.*
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class ParserSpec extends AnyWordSpecLike with Matchers with EitherValues {

  private val starParser = Parser.partial("starParser") {
    case StrToken("*") +: tail => ParserResult("good", tail)
  }

  private val comaParser: Parser[Symbols.Coma.type] = Parser.token(Symbols.Coma)

  "token" in {
    val inOut = List(
      ("*", Left("")),
      (",", Right(ParserResult(Symbols.Coma, Seq.empty))),
    )

    inOut.foreach { (in, expectation) =>
      val result = Preprocessor(in).flatMap(comaParser.apply)
      expectation match {
        case Right(value) =>
          result.value shouldBe value
        case Left(_) =>
          result.isLeft shouldBe true
      }
    }
  }

  "andThen" in {
    val inOut = List(
      ("*", Left("")),
      ("*,", Right(ParserResult(("good", Symbols.Coma), Seq.empty))),
    )

    inOut.foreach { (in, expectation) =>
      val result = Preprocessor(in).flatMap(starParser.andThen(comaParser).apply)
      expectation match {
        case Right(value) =>
          result.value shouldBe value
        case Left(_) =>
          result.isLeft shouldBe true
      }
    }
  }

  "option" in {
    val inOut = List(
      (".", Right(ParserResult(None, Seq(StrToken("."))))),
      ("*", Left("")),
      ("*,", Right(ParserResult(Some(Symbols.Coma), Seq.empty)))
    )

    inOut.foreach { (in, expectation) =>
      val result = Preprocessor(in).flatMap(Parser.option(starParser)(_)(_ => comaParser))
      expectation match {
        case Right(value) =>
          result.value shouldBe value
        case Left(_) =>
          result.isLeft shouldBe true
      }
    }
  }

  "enclosed" in {
    val inOut = List(
      ("*", Left("")),
      ("(*", Left("")),
      ("(*)", Right(ParserResult("good", Seq.empty)))
    )

    inOut.foreach { (in, expectation) =>
      val result = Preprocessor(in).flatMap(starParser.enclosed.apply)
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
      val result = Preprocessor(in).flatMap(starParser.orEnclosed.apply)
      expectation match {
        case Right(value) =>
          result.value shouldBe value
        case Left(_) =>
          result.isLeft shouldBe true
      }
    }
  }
}
