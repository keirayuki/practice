import datastructures._

object Example_List {
    def main(args: Array[String]): Unit = {
        val ex1: List[Nothing] = Nil
        val ex2: List[Double] = Cons(1.0,Nil)
        val ex3: List[String] = Cons("a", Cons("b", Nil))

        // Listの表示
        println(ex1) 
        println(ex2)
        println(ex3)

        // 関数の使用
        val ex4: Int = List.sum(Cons(1,Cons(2,Cons(3,Nil))))
        val ex5: Double = List.product(Cons(1.0,Cons(2.0,Cons(3.0,Nil))))
        val ex6: List[String] = List.apply("Hello","World","!")
        
        println(ex4)
        println(ex5)
        println(ex6)
    }
}