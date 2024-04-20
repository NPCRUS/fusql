import org.scalatest.wordspec.AnyWordSpecLike
import Ast.*
import Ast.CondOperator.*
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers

class ConfigSpec extends AnyWordSpecLike with EitherValues with Matchers {

  "test forbidden keywords" in {
    val config = Config.empty
      .on("users") { table =>
        table
          .forbid("password")
          .filter(BasicBoolExpr(Gt, ColumnRef("company_id", None), IntLiteral(120)))
      }

    val inOut = List(
      ("SELECT password FROM users", true),
      ("SELECT users.password FROM users", true),
      ("SELECT u.password FROM users as u", true),
      ("SELECT t1.password FROM SELECT password FROM users as t1 as t2", true),
    )

    inOut.foreach { (in, out) =>
      val result = Applicator.run(config, in)
      println(result)
      result.isLeft shouldBe out
    }
  }

}
