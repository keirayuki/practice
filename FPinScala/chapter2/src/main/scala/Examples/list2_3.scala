object MyModule4 {

  def abs(n : Int): Int = {
    if (n < 0) -n
    else n
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
        if (n <= 0) acc
        else go(n-1, n*acc)

    go(n,1)
  }

  private def formatAbs(x: Int) = {
    val msg = "The value of %d is %d"
    msg.format(x,abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  //formatFacorialとFormatAbsを一括にしたもの
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name,n,f(n))
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
    println(formatFactorial(7))

}