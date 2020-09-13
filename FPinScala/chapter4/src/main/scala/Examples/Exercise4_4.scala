import errorhandling._

class Exercise4_4 {
    /*
     Exercise 4.4
      Optionのリストを１つのOptionにまとめるsequence関数を記述せよ。
      新しいOptionには、元のリストに含まれている全てのSome値のリストが含まれている。
      元のリストにNoneが含まれていた場合、この関数の結果はNoneになる。
    */
    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap(a => sequence(t) map (a :: _)) 
    }
      
}
