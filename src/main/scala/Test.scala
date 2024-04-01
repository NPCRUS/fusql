import Ast.{Coma, CondOperator, Literal, Symbols, Token}
import Parser.*

object Test extends App {

   // val result = Parser.parse("SELECT id, name, zhopa FROM table WHERE name = 'zhopa' and id >= 2 or zhopa is null")
   // println(result)

   val test1 = "name, name as n1, t.name, t.name as n2 SELECT FROM"
   val preprocessed = Parser.preprocess(test1)
   println(preprocessed)
   val parser = parseSeq(columnParser, Symbols.FROM)
   val result = preprocessed.map(parser)
   println(result)
}


// where 1=1 
//  and name = 'test'
//  or id = 11