object Lists {
  def listFold[A, B](z: B, f: (A, B) => B, list: List[A]): B =
    list match {
      case Nil => z
      case head :: tail => f(head, listFold(z, f, tail))
    }

  val list4 = List(3, 2, 5, 8)

  // Typinferenz in Scala: für jedes Argument innerhalb eines Klammernpaars separat
  // => Typ unbekannt, auf den sich + hier bezieht
  // val list4Sum = listFold(0, _+_, list4)
  val list4Sum = listFold[Int, Int](0, _+_, list4)

  // Currifizierung:
  def listFold2[A, B](list: List[A])(z: B, f: (A, B) => B): B =
    list match {
      case Nil => z
      case head :: tail => f(head, listFold2(tail)(z, f))
    }

  val list4Sum2 = listFold2(list4)(0, _+_)

  // idiomatische Version:
  extension[A] (list: List[A])
    def myFold[B](z: B)(f: (A, B) => B): B = // fold gibt's schon
      list match {
        case Nil => z
        case head :: tail => f(head, tail.myFold(z)(f))
      }

  // Wunsch:
  val list4Sum3 = list4.fold(0) { (a, b) => a + b }

}