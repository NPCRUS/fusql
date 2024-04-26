import org.scalatest.EitherValues
import org.scalatest.wordspec.AnyWordSpecLike
import Printer.queryPrinter
import org.scalatest.matchers.should.Matchers

class PrinterSpec extends AnyWordSpecLike with Matchers with EitherValues {
  "printer" in {
    val input = "select name from table WHERE zhopa > 10 OR table.isTrue AND table.zhepa = 0"
    val parsed = Parsers.parse(input).value
    val printed1 = queryPrinter.print(parsed)
    val parsed2 = Parsers.parse(printed1).value
    val printed2 = queryPrinter.print(parsed2)

    println(printed1)
    println(printed2)
    printed1 shouldBe printed2
  }
}
