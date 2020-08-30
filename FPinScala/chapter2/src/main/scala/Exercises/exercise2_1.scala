class exercise2_1{

    def fib(n: Int): Int = {
        def go(n: Int, a: Int, b:Int): Int = {
            if (n <= 1) a
            else go(n-1,b,a+b)
        }
        go(n,0,1)
    }
}