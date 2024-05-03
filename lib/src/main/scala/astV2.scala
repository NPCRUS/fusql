import java.time.LocalDateTime

object astV2 {
  import ComparisonOperator.*
  import LogicalOperator.*

  sealed trait Literal

  object Literal {
    case class Bool(value: Boolean) extends Literal

    case class Int(value: scala.Int) extends Literal

    case class Str(value: String) extends Literal

    case class Timestamp(value: LocalDateTime) extends Literal
  }

  // ----------------------------------------------------------------

  opaque type ColumnName = String

  type ColumnFrom = TableName | Schema
  case class Column(column: ColumnName, from: ColumnFrom)

  case class WildcardFrom(from: ColumnFrom)

  type SelectAliasInput = ColumnName | Column | AggFunction | Query
  case class SelectAlias(input: SelectAliasInput, alias: String)

  type AggFunctionArg = ColumnName | Literal
  case class AggFunction(name: String, args: Seq[AggFunctionArg])

  type ColumnRef = Column | ColumnName

  // ----------------------------------------------------------------

  opaque type TableName = String

  case class Schema(table: TableName)

  type TableAliasInput = TableName | Schema | Query

  case class TableAlias(input: TableAliasInput, alias: String)

  type TableRef = TableName | Schema
  sealed trait Join
  object Join {
    type JoinRef = TableRef | TableAlias
    case class Inner(table: JoinRef, on: WhereExpr) extends Join
    case class Left(table: JoinRef, on: WhereExpr) extends Join
    case class Right(table: JoinRef, on: WhereExpr) extends Join
    case class Full(table: JoinRef, on: WhereExpr) extends Join
  }

  // ----------------------------------------------------------------

  sealed trait ComparisonOperator

  object ComparisonOperator {
    type ComparisonOperand = Column | Literal

    case class Eq(left: ColumnRef, right: ComparisonOperand) extends ComparisonOperator
    case class NotEq(left: ColumnRef, right: ComparisonOperand) extends ComparisonOperator
    case class Gt(left: ColumnRef, right: ComparisonOperand) extends ComparisonOperator
    case class Lt(left: ColumnRef, right: ComparisonOperand) extends ComparisonOperator
    case class GtOrEq(left: ColumnRef, right: ComparisonOperand) extends ComparisonOperator
    case class LtOrEq(left: ColumnRef, right: ComparisonOperand) extends ComparisonOperator
    case class Between(base: ColumnRef, left: ComparisonOperand, right: ComparisonOperand) extends ComparisonOperator
    case class IsNull(left: ColumnRef) extends ComparisonOperator
    case class In(left: ColumnRef, right: Query | Seq[Literal]) extends ComparisonOperator
    case class Like(left: ColumnRef, right: String) extends ComparisonOperator
    case class Ilike(left: ColumnRef, right: String) extends ComparisonOperator
  }

  // ----------------------------------------------------------------

  sealed trait LogicalOperator

  object LogicalOperator {
    type LogicalOperand = ComparisonOperator | ColumnFrom | LogicalOperator

    case class And(left: LogicalOperand, right: LogicalOperand)
    case class Or(left: LogicalOperand, right: LogicalOperand)

    type NotOperand = Between | IsNull | In | Like | Ilike | And | Or
    case class Not(inner: NotOperand)
  }

  // ----------------------------------------------------------------

  type OrderByExpr = ColumnRef | AggFunction | Literal.Int

  enum Ordering {
    case Asc
    case Desc
  }
  enum NullOrdering {
    case NullsFirst
    case NullsLast
  }

  case class OrderBy(input: OrderByExpr, ordering: Ordering, nullOrdering: NullOrdering)

  // ----------------------------------------------------------------

  case object Wildcard
  
  type SelectExpr = Wildcard.type | WildcardFrom | ColumnName | ColumnFrom | SelectAlias
  
  type FromExpr = TableName | Schema | TableAlias | Join
  
  type WhereExpr = ComparisonOperator | LogicalOperator
  
  case class Skip(value: Int)
  
  case class Take(value: Int)
  
  case class Query(distinct: Boolean,
                   select: Seq[SelectExpr],
                   from: Seq[FromExpr],
                   where: Option[WhereExpr],
                   skip: Option[Skip],
                   take: Option[Take],
                   orderBy: OrderBy)
}