import org.scalatest.wordspec.AnyWordSpecLike
import Ast.*
import Ast.CondOperator.*
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import Printer._

class ConfigSpec extends AnyWordSpecLike with EitherValues with Matchers {

  "config" should {
    "check select for forbidden keywords" in {
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
        val result = ConfigApplicator.run(config, in)
        println(result)
        result.isLeft shouldBe out
      }
    }

    "check where for forbidden keywords or usage of columns used in contextFilter" in {
      val config = Config.empty
        .on("users") { table =>
          table
            .forbid("password")
            .filter(BasicBoolExpr(Gt, ColumnRef("company_id", None), IntLiteral(120)))
        }

      val inOut = List(
        ("SELECT id FROM users WHERE users.password", true),
        ("SELECT id FROM users as u WHERE 'test' = u.password", true),
        ("SELECT id FROM users as u WHERE id BETWEEN u.password AND 'zhopa'",                                        true),
        ("SELECT id FROM users as u WHERE id = 1 and users.password = 'zhopa'", true),
        ("SELECT id FROM users WHERE users.company_id", true),
        ("SELECT id FROM users as u WHERE 1 = 1 and users.name = 'zhopa' and u.company_id = 120", true),
      )

      inOut.foreach { (in, out) =>
        val result = ConfigApplicator.run(config, in)
        println(result)
        result.isLeft shouldBe out
      }
    }
    
    "apply filter" in {
      val config = Config.empty
        .on("users") { table =>
          table
            .forbid("password")
            .filter(BasicBoolExpr(Gt, ColumnRef("company_id", None), IntLiteral(120)))
        }
      
      val inOut = List(
        (
          "SELECT id FROM users WHERE name = 'zhopa'",
          Query(Seq(ColumnRef("id", None)), StrToken("users"), Some(ComplicatedBoolExpr(
            And,
            BasicBoolExpr(Gt, ColumnRef("company_id", None), IntLiteral(120)),
            BasicBoolExpr(Equals, ColumnRef("name", None), StringLiteral("zhopa"))
          )))
        )
      )

      inOut.foreach { (in, out) =>
        val result = ConfigApplicator.run(config, in)
        println(Printer.queryPrinter.print(result.value))
        result.value shouldBe out
      }
    }
  }

}
