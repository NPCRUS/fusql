import fastparse.*
import SingleLineWhitespace.*
import astV2.*
import Literal.*
import ComparisonOperator.*

import java.time.LocalDateTime

object parserV2 {

  def literalBoolP[$: P]: P[Bool] = P {
    ("true" | "false").!.map {
      case "true" => Bool(true)
      case "false" => Bool(false)
    }
  }

  def literalIntP[$: P]: P[Int] = P {
    CharIn("0-9").rep(1).!.map(v => Int(v.toInt))
  }

  // TODO: regexp all chars, except "'"
  def literalStrP[$: P]: P[Str] = P {
    ("'" ~ CharIn("a-zA-Z").rep.! ~ "'").map(Str.apply)
  }

  private def n[$: P](n: scala.Int): P[scala.Int] = P(CharIn("0-9").rep(exactly = n).!).map(_.toInt)
  def literalTimestampP[$: P]: P[Timestamp] = P {
    "'" ~ n(4) ~ "-" ~ n(2) ~ "-" ~ n(2) ~ n(2) ~ ":" ~ n(2) ~ ":" ~ n(2) ~ "'"
  }.map { case (year, month, day, hour, minute, second) =>
    Timestamp(LocalDateTime.of(year, month, day, hour, minute, second))
  }

  def literalP[$: P]: P[Literal] = P(
    literalBoolP | literalIntP | literalTimestampP | literalStrP
  ).opaque("literal")

  // --------------------------------------------------

  // TODO: add other allowed special symbols like '_'
  private def nameId[$: P]: P[String] = P {
    lazy implicit val wts: NoWhitespace.noWhitespaceImplicit.type = NoWhitespace.noWhitespaceImplicit
    CharIn("a-zA-Z0-9", "\\_").rep.!
  }

  private def period[$: P] = P(".")

  private def asterisk[$: P] = P("*")

  private def as[$: P] = P(StringInIgnoreCase("as"))

  private def coma[$: P] = P(",")

  // --------------------------------------------------

  def schemaP[$: P]: P[Schema] = P(
    nameId ~ period ~ nameId
  ).map((schema, table) => Schema(schema, TableName(table)))

  def columnP[$: P]: P[Column] = P(
    (schemaP ~ period ~ nameId)
      .map((schema, column) => Column(schema, ColumnName(column))) |
    (nameId ~ period ~ nameId)
      .map((table, column) => Column(TableName(table), ColumnName(column)))
  )

  def wildcardFromP[$: P]: P[WildcardFrom] = P(
    (schemaP ~ period ~ asterisk).map(WildcardFrom.apply) |
      (nameId ~ period ~ asterisk).map(table => WildcardFrom(TableName(table)))
  )

  def columnRefP[$: P]: P[ColumnRef] = P(columnP | nameId.map(ColumnName.apply))

  private def aggFunctionArgP[$: P]: P[AggFunctionArg] = P(literalP | columnRefP)
  // TODO: complete list of agregation functions
  private def aggFunctions: Seq[String] = Seq("count", "sum", "avg")
  def aggFunctionP[$: P]: P[AggFunction] = P(
    StringInIgnoreCase("count", "sum", "avg").! ~ "(" ~ aggFunctionArgP.rep(1, sep = coma) ~ ")"
  ).map(AggFunction.apply)

  // TODO: add queryP
  def selectAliasP[$: P]: P[SelectAlias] = P(
    (aggFunctionP | columnRefP) ~ as ~ nameId
  ).map(SelectAlias.apply)

  // --------------------------------------------------

  def tableRefP[$: P]: P[TableRef] = P(schemaP | nameId.map(TableName.apply))

  // TODO: add queryP
  def tableAliasP[$: P]: P[TableAlias] = P(
    tableRefP ~ as ~ nameId
  ).map(TableAlias.apply)

  // TODO: join

  // --------------------------------------------------

  private def comparisonOperandP[$: P]: P[ComparisonOperand] = P(literalP | columnRefP)
  private def between[$: P] = P(StringInIgnoreCase("between"))
  private def and[$: P] = P(StringInIgnoreCase("and"))
  private def or[$: P] = P(StringInIgnoreCase("or"))
  private def not[$: P] = P(StringInIgnoreCase("not"))
  private def is[$: P] = P(StringInIgnoreCase("is"))
  private def `null`[$: P] = P(StringInIgnoreCase("null"))
  private def like[$: P] = P(StringInIgnoreCase("like"))
  private def ilike[$: P] = P(StringInIgnoreCase("ilike"))
  //TODO: add all supported symbols
  private def searchStr[$: P]: P[String] = P(CharIn("a-zA-Z%")).!
  
  //TODO: add IN with query
  def comparisonOperatorP[$: P]: P[ComparisonOperator] = P(
    (columnRefP ~ "=" ~ comparisonOperandP).map(Eq.apply) |
      (columnRefP ~ ("=" | "<>") ~ comparisonOperandP).map(NotEq.apply) |
      (columnRefP ~ ">" ~ comparisonOperandP).map(Gt.apply) |
      (columnRefP ~ "<" ~ comparisonOperandP).map(Lt.apply) |
      (columnRefP ~ ">=" ~ comparisonOperandP).map(GtOrEq.apply) |
      (columnRefP ~ "<=" ~ comparisonOperandP).map(LtOrEq.apply) |
      (columnRefP ~ between ~ comparisonOperandP ~ and ~ comparisonOperandP).map(Between.apply) |
      (columnRefP ~ is ~ `null`).map(IsNull.apply) |
      (columnRefP ~ like ~ searchStr).map(Like.apply) |
      (columnRefP ~ ilike ~ searchStr).map(Ilike.apply)
  )
}
