import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import errorhandling._


class Exercise4_1 extends AnyFlatSpec with Diagrams {
  // Test用関数の定義
  def ex4(x: Double): Option[Double] = {
    if(x == 10) None 
    else Some(x)
  }
  
  // Test用変数の定義
  val ex1: Option[Double] = Some(100.0)
  val ex2: Option[Double] = Some(10.0)
  val ex3: Option[Double] = None

  // map関数のテスト
  "map関数" should "OptionがNone出ない場合は関数を適用。" in {
    assert(ex1.map(x => x*2) === Some(200.0))
  }

  // getOrElse関数のテスト　
  "getOrElse関数" should "OptionのSomeケースの結果を返す。Noneの場合は、default値をかえす。" in {
      assert(ex2.getOrElse(5.0) === 10.0)
      assert(ex3.getOrElse(5.0) === 5.0)
  }

  // flatMap関数のテスト　
  "flatMap関数" should "OptionがNoneでない場合は、失敗する可能性があるfを適用。" in {
      assert(ex1.flatMap(ex4) === ex1)
      assert(ex2.flatMap(ex4) === None)
  }

  // orElse関数のテスト
  "orElse関数" should "１つ目のOptionが定義されている場合はそれを返し、そうでない場合は２つ目のOptionを返す。" in {
      assert(ex1.orElse(ex2) === ex1)
      assert(ex3.orElse(ex2) === ex2)
  }

  // filter関数のテスト
  "filter関数" should "値がfの条件を満たさない場合は、SomeをNoneに変換" in {
      assert(ex1.filter(_ > 10.0) === ex1)
      assert(ex2.filter(_ > 10.0) === None)
  }

}