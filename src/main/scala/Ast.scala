object Ast {
  
  sealed trait Token

  enum Symbols(token: String) extends Token {
    case Select extends Symbols("select")
    case From extends Symbols("from")
    case Where extends Symbols("where")
    case Take extends Symbols("take")
    case Skip extends Symbols("skip")
    case As extends Symbols("as")
    case Coma extends Symbols(",")
    case BlockOpen extends Symbols("(")
    case BlockClose extends Symbols(")")

    def eq(str: String): Boolean = token == str.toLowerCase
    
    def repr: String = token
  }

  sealed trait AndOr

  object CondOperator {
    def findOperator(str: String): Option[CondOperator] =
      CondOperator.values.find(_.toString == str.toLowerCase)
  }

  enum CondOperator(token: String) extends Token {

    def eq(str: String): Boolean = this.token == str.toLowerCase
    def print: String = this.token

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
  final case class FunctionCall(func: String, args: Seq[Literal | ColumnRef]) extends Expr

  // TODO: joins
  // TODO: skip/take
  final case class Query(select: Seq[SelectRef], from: StrToken | TableAlias, where: Option[BoolExpr])

  // ----------------------------

  type BooleanExprOperand = Expr | ColumnRef

  case class BasicBoolExpr(operator: CondOperator, a: BooleanExprOperand, b: BooleanExprOperand)

  case class Between(base: BooleanExprOperand, a: BooleanExprOperand, b: BooleanExprOperand)

  // WHERE table1.name LIKE '%zhopa%' AND (table1.name LIKE '%zhopa%' OR table1.a BETWEEN count(table1.b) AND 100)
  case class ComplicatedBoolExpr(operator: AndOr, a: BoolExpr, b: BoolExpr)

  type BoolExpr = BooleanLiteral | ColumnRef | BasicBoolExpr | Between | ComplicatedBoolExpr

  final case class ColumnRef(column: String, tableRef: Option[String])

  final case class Alias(input: Expr | ColumnRef, alias: String)

  final case class TableAlias(input: Expr | Query | StrToken, alias: String)

  // TODO: add Expr
  type SelectRef = ColumnRef | Alias
  
  // final case class Skip(v: Int) extends Expr
  
  // final case class Take(v: Int) extends Expr
}
