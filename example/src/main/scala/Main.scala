import Ast.{BasicBoolExpr, ColumnRef, ComplexBoolExpr, CondOperator, IntLiteral, StringLiteral, Symbols}
import Ast.CondOperator.{Equals, Is}
import Db.*
import slick.jdbc.PostgresProfile.api.*
import slick.jdbc.SQLActionBuilder
import slick.jdbc.SetParameter.SetNothing
import upickle.default.*

import scala.concurrent.Await
import scala.concurrent.duration.*

object Main extends cask.MainRoutes {
  def config(orga: String): Config =
    Config.empty
      .on("library") { c =>
        c.filter(
          ComplexBoolExpr(
            CondOperator.Or,
            BasicBoolExpr(Equals, ColumnRef("orga", Some("library")), StringLiteral(orga)),
            BasicBoolExpr(Is, ColumnRef("orga", Some("library")), Symbols.Null)
          )
        )
      }

  @cask.post("query")
  def query(request: cask.Request, orga: String) = {
    val sqlQueryString = String.valueOf(request.data.readAllBytes().map(_.toChar))

    val result = ConfigApplicator.run(config(orga), sqlQueryString)
    result.map(Printer.queryPrinter.print(_)).merge
    result match
      case Left(error) =>
        error
      case Right(sqlAst) =>
        val fields = sqlAst.select.map {
          case Ast.ColumnRef(cl, _) => cl
          case Ast.Alias(_, cl) => cl
        }

        println(fields)
        write(
          Await.result(
            db.run {
              val sqlQuery = Printer.queryPrinter.print(sqlAst)
              println(sqlQuery)
              SQLActionBuilder(sqlQuery, SetNothing).as[ujson.Obj](getResultDynamic(fields))
            }
            , 10.seconds)
        )
  }

  Await.result(Db.setupFuture, 10.seconds)
  initialize()
}
