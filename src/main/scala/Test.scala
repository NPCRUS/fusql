import Ast.{CondOperator, StrToken, Symbols, Token}
import Parsers.*

object Test extends App {

   val result = Parsers.parse("SELECT id, name, zhopa FROM table WHERE name = 'zhopa' and id >= 2 or zhopa is null")
   println(result)
}


// where 1=1 
//  and name = 'test'
//  or id = 11