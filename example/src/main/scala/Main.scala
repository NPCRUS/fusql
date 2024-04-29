import Ast.{BasicBoolExpr, ColumnRef, IntLiteral}
import Ast.CondOperator.Equals
import Db.*
import slick.jdbc.PostgresProfile.api.*
import slick.jdbc.SQLActionBuilder
import slick.jdbc.SetParameter.SetNothing
import upickle.default.*

import scala.concurrent.Await
import scala.concurrent.duration.*

object Main extends cask.MainRoutes {
  def config(companyId: Int): Config =
    Config.empty
      .on("users") { c =>
        c.forbid("password")
          .filter(BasicBoolExpr(Equals, ColumnRef("company_id", None), IntLiteral(companyId)))
      }

  @cask.post("query")
  def query(request: cask.Request, companyId: Int) = {
    val sqlQueryString = String.valueOf(request.data.readAllBytes().map(_.toChar))

    val result = ConfigApplicator.run(config(companyId), sqlQueryString)
    result.map(Printer.queryPrinter.print(_)).merge
    result match
      case Left(error) =>
        error
      case Right(value) =>
        write(
          Await.result(
            db.run {
              val sqlQuery = Printer.queryPrinter.print(value)
              println(sqlQuery)
              SQLActionBuilder(sqlQuery, SetNothing).as[Library]
            }
            , 10.seconds)
        )
  }

  Await.result(Db.setupFuture, 10.seconds)
  initialize()
}
