import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import Ast.{StrToken => S, *}
import Ast.Symbols.*
import Ast.CondOperator.*

class PreprocessorSpec extends AnyWordSpecLike with Matchers with EitherValues {
  "enrichWithToken" in {
    val result = Preprocessor("=test<=()bla>=,bla,!=bla)>zhopa")

    result.value shouldBe Seq(
      Equals,
      S("test"),
      LessThan,
      BlockOpen,
      BlockClose,
      S("bla"),
      GtThan,
      Coma,
      S("bla"),
      Coma,
      NotEquals,
      S("bla"),
      BlockClose,
      Gt,
      S("zhopa"))
  }
}
