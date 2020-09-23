package testing

import state._
import Prop._
import Gen._

/* Exercise 8.3
* Propの表現が以下であると仮定して、
* &&をPropのメソッドとして実装せよ。
trait Prop { 
  def check: Boolean   
  def &&(p: Prop): Prop = new Prop {
    def check = Prop.this.check && p.check
  }
 }
 */

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

case class Gen[+A](sample: State[RNG, A]) {
}

object Gen {
  /* Exercise 8.4
  * Genのこの表現を使って、startからstopExclusiveの範囲内の整数を生成する
  * Gen.choose実装せよ。
  */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = 
    Gen(State( rng => RNG.nonNegativeInt(rng) match {
      case (n, rng2) => (start + n % (stopExclusive - start), rng2)
    }))

  /* Exercise 8.5
  * unit,boolean,listOfNを実装せよ
  */
  def unit[A](a: => A): Gen[A] =
    Gen(State(rng => (a,rng)))

  def boolean: Gen[Boolean] =
    Gen(State(rng => RNG.boolean(rng)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))


}
