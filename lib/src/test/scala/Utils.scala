import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers

object Utils extends Matchers with EitherValues {
  def checkParser[T](parser: Parser[T])(inOut: Seq[(String, ErrOr[ParserResult[T]])]): Unit =
    println(parser.name)
    inOut.foreach { (in, expectation) =>
      val result = Preprocessor(in).flatMap(parser.apply)
      expectation match
        case Left(value) =>
          result.isLeft shouldBe true
        case Right(value) =>
          result.value shouldBe value
    }
}
