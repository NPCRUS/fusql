object Ast {
  
  sealed trait Token

  enum Symbols extends Token {
    case SELECT, FROM, WHERE, TAKE, SKIP, AS

    def eq(str: String): Boolean = this.toString == str || this.toString.toLowerCase == str
  }

  case object Coma extends Token

  sealed trait AndOr

  object CondOperator {
    def findOperator(str: String): Option[CondOperator] =
      CondOperator.values.find(_.toString == str.toLowerCase)
  }

  enum CondOperator(token: String) extends Token {

    def eq(str: String): Boolean = this.token == str.toLowerCase

    case And extends CondOperator("and") with AndOr
    case Or extends CondOperator("or") with AndOr
    case Equals extends CondOperator("=")
    case NotEquals extends CondOperator("!=")
    case Gt extends CondOperator(">")
    case GtThan extends CondOperator(">=")
    case Less extends CondOperator("<")
    case LessThan extends CondOperator("<=")
    case Between extends CondOperator("between")
    case Like extends CondOperator("like")
    case In extends CondOperator("in")
    case Is extends CondOperator("is")
  }

  final case class StrToken(v: String) extends Token

  sealed trait Expr

  // --------- LITERALS
  sealed trait Literal extends Expr

  case class BooleanLiteral(value: Boolean) extends Literal

  case class IntLiteral(value: Int) extends Literal

  case class StringLiteral(value: String) extends Literal
  // ------------------------


  // -------- EXPRESSIONS -----------
  // TODO: left part might not be only column ref
  final case class BooleanExpr(operator: CondOperator, a: ColumnRef, b: BooleanExpr | Expr | ColumnRef | BooleanLiteral) extends Expr
  
  final case class Function(func: String, args: (Literal | ColumnRef)*) extends Expr

  final case class Query(select: Select, from: From, where: Option[List[Where]]) extends Expr

  // ----------------------------

  final case class ColumnRef(column: String, tableRef: Option[String])

  final case class Alias(input: Expr | ColumnRef, alias: String)

  type SelectRef = Expr | ColumnRef | Alias

  final case class Select(columns: Seq[StrToken])

  final case class From(v: StrToken)

  trait Cond

  case class SimpleCond(operator: AndOr, left: StrToken, right: StrToken) extends Cond

  case class Between(left: StrToken, leftBetween: StrToken, rightBetween: StrToken) extends Cond {
    def operator: CondOperator = CondOperator.Between
  }
  
  sealed trait Where extends Expr {
    def head: Cond
  }
  
  case class WhereAnd(head: Cond) extends Where
  
  case class WhereOr(head: Cond) extends Where
  
  // final case class Skip(v: Int) extends Expr
  
  // final case class Take(v: Int) extends Expr
}
