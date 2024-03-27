
object Test extends App {

   val result = Parser.parse("SELECT id, name, zhopa FROM table WHERE name = 'zhopa' and id >= 2 or zhopa is null")
   println(result)
}


// where 1=1 
//  and name = 'test'
//  or id = 11