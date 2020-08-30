import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams

class exerxise2_2_test extends AnyFlatSpec with Diagrams {

  def ordered(n1: Int, n2: Int): Boolean = {
    if (n1 < n2) true
    else false 
  }

  val exe2_2 = new exercise2_2

  "ソート関数" should "ソートされているものにtrueを返す" in {
    assert(exe2_2.isSorted(Array(1,4,5,10,20,99),ordered) === true)
    assert(exe2_2.isSorted(Array(2,10,44,60,80,109),ordered) === true)
    assert(exe2_2.isSorted(Array(6,9,10,33,70,90),ordered) === true)
  }

  "ソート関数" should "ソートされていないものにfalseを返す" in {
    assert(exe2_2.isSorted(Array(19,22,3,28,99),ordered) === false)
    assert(exe2_2.isSorted(Array(11,100,20,30,34,30,200),ordered) === false)
    assert(exe2_2.isSorted(Array(1,2,3,10,99,100,80),ordered) === false)
  }
}