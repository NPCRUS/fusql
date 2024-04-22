import Ast.{BasicBoolExpr, ColumnRef, IntLiteral}
import Ast.CondOperator.Equals

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
  }

  initialize()
}
