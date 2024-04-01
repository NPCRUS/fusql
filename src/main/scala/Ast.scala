object Ast {
  
  sealed trait Token

  enum Symbols extends Token {
    case SELECT, FROM, WHERE, TAKE, SKIP, AS

    def eq(str: String): Boolean = this.toString == str || this.toString.toLowerCase == str
  }

  case object Coma extends Token

  object CondOperator {
    def findOperator(str: String): Option[CondOperator] =
      CondOperator.values.find(_.toString == str.toLowerCase)
  }

  enum CondOperator(token: String) extends Token {

    def eq(str: String): Boolean = this.token == str.toLowerCase

    case And extends CondOperator("and")
    case Or extends CondOperator("or")
  }

  object ComparisonOperator {
    def findOperator(str: String): Option[ComparisonOperator] =
      ComparisonOperator.values.find(_.toString == str.toLowerCase)
  }

  enum ComparisonOperator(token: String) extends Token {

    def eq(str: String): Boolean = this.token == str.toLowerCase

    case Equals extends ComparisonOperator("=")
    case NotEquals extends ComparisonOperator("!=")
    case Gt extends ComparisonOperator(">")
    case GtThan extends ComparisonOperator(">=")
    case Less extends ComparisonOperator("<")
    case LessThan extends ComparisonOperator("<=")
    case Between extends ComparisonOperator("between")
    case Like extends ComparisonOperator("like")
    case In extends ComparisonOperator("in")
    case Is extends ComparisonOperator("is")
  }
  
  sealed trait Expr

  final case class Literal(v: String) extends Token with Expr

  final case class Column(column: String, tableRef: Option[String], alias: Option[String]) extends Expr

  final case class Function(func: String, args: (Literal | Column)*) extends Expr

  final case class Query(select: Select, from: From, where: Option[List[Where]]) extends Expr

  final case class Select(columns: Seq[Literal])
  
  final case class From(v: Literal)

  trait Cond

  case class SimpleCond(operator: ComparisonOperator, left: Literal, right: Literal) extends Cond

  case class Between(left: Literal, leftBetween: Literal, rightBetween: Literal) extends Cond {
    def operator: ComparisonOperator = ComparisonOperator.Between
  }
  
  sealed trait Where extends Expr {
    def head: Cond
  }
  
  case class WhereAnd(head: Cond) extends Where
  
  case class WhereOr(head: Cond) extends Where
  
  // final case class Skip(v: Int) extends Expr
  
  // final case class Take(v: Int) extends Expr
}
