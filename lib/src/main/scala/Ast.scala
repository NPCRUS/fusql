object Ast {
  
  sealed trait Token {
    def repr: String
  }

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

  enum CondOperator(token: String) extends Token {
    
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

    def eq(str: String): Boolean = this.token == str.toLowerCase
    
    def repr: String = token
  }

  final case class StrToken(v: String) extends Token {
    override def repr: String = v
  }

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

  // TODO: operand 'a' can be only column ref, there's no point of having left side as expression
  // TODO: add support for IN, where
  type BooleanExprOperand = Expr | ColumnRef

  case class BasicBoolExpr(operator: CondOperator, a: BooleanExprOperand, b: BooleanExprOperand)

  case class BetweenExpr(base: BooleanExprOperand, a: BooleanExprOperand, b: BooleanExprOperand)
  
  case class ComplicatedBoolExpr(operator: AndOr, a: BoolExpr, b: BoolExpr)

  type BoolExpr = ColumnRef | BasicBoolExpr | BetweenExpr | ComplicatedBoolExpr

  final case class ColumnRef(column: String, tableRef: Option[String])

  final case class Alias(input: Expr | ColumnRef, alias: String)

  final case class TableAlias(input: Expr | Query | StrToken, alias: String)

  type SelectRef = ColumnRef | Alias
  
  // final case class Skip(v: Int) extends Expr
  
  // final case class Take(v: Int) extends Expr
}
