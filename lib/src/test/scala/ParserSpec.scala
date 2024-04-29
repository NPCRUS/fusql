import Ast.*
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import Utils._

class ParserSpec extends AnyWordSpecLike with Matchers with EitherValues {

  private val starParser = Parser.partial("starParser") {
    case StrToken("*") +: tail => ParserResult("good", tail)
  }

  private val comaParser: Parser[Symbols.Coma.type] = Parser.token(Symbols.Coma)

  "token" in {
    val inOut: Seq[(String, ErrOr[ParserResult[Symbols.Coma.type]])] = List(
      ("*", Left("")),
      (",", Right(ParserResult(Symbols.Coma, Seq.empty))),
    )

    checkParser(comaParser)(inOut)
  }

  "andThen" in {
    val inOut: Seq[(String, ErrOr[ParserResult[(String, Symbols.Coma.type)]])] = List(
      ("*,", Right(ParserResult(("good", Symbols.Coma), Seq.empty))),
      ("*", Left("")),
    )

    checkParser(starParser.andThen(comaParser))(inOut)
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

    checkParser(starParser.enclosed)(inOut)
  }

  "orEnclosed" in {
    val inOut = List(
      ("*", Right(ParserResult("good", Seq.empty))),
      ("(*", Left("")),
      ("(*)", Right(ParserResult("good", Seq.empty)))
    )

    checkParser(starParser.orEnclosed)(inOut)
  }

  "seq" in {
    val inOut = List(
      ("*, * FROM", Right(ParserResult(List("good", "good"), Seq.empty))),
      ("*,,", Left(""))
    )

    checkParser(Parser.seq(starParser, Symbols.From))(inOut)
  }
}
