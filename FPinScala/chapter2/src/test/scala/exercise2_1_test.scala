import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams

class exerxise2_1_test extends AnyFlatSpec with Diagrams {

  val exe2_1 = new exercise2_1

  "fib関数" should "任意の数のフィボナッチ数列を計算できる" in {
    assert(exe2_1.fib(1) === 0)
    assert(exe2_1.fib(2) === 1)
    assert(exe2_1.fib(5) === 3)
    assert(exe2_1.fib(20) === 4181)
  }
}