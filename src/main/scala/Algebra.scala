object Algebra {

  enum MyList[+A] { // Varianzannotation
    case Empty
    case Cons(first: A, rest: MyList[A])

  }

  import MyList._

  extension[A] (list: MyList[A]) {
    def append(other: MyList[A]): MyList[A] =
      list match {
        case Empty => other
        case Cons(first, rest) => Cons(first, rest.append(other))
      }
  }

  enum Foo extends Semigroup[Foo] {
    case Bar
    case Baz

    override def combine(other: Foo): Foo = Baz
  }



  trait Semigroup[A] {
    def combine(other: A): A
  }

  


  trait Monoid[A] {
    val neutral: A  // bedingt, daß der Monoid ein separates Objekt ist
    // auch noch möglich: hier extension, dann kann separates combineM wegfallen
    def combine(a1: A, a2: A): A
  }

  given fooMonoid: Monoid[Foo] = new Monoid[Foo] {
    override val neutral = Foo.Bar
    override def combine(a1: Foo, a2: Foo): Foo = Foo.Baz
  }

  /*
  extension (foo1: Foo) {
    def combine(foo2: Foo): Foo = fooMonoid.combine(foo1, foo2)
  }
    */
  /*
  extension[A] (a1: A) {
    def combine(a2: A, monoid: Monoid[A]): A = monoid.combine(a1, a2)
  }
  */
  /*
  extension[A] (a1: A) {
    def combine(a2: A)(monoid: Monoid[A]): A = monoid.combine(a1, a2)
  }
  val foo1 = Foo.Bar.combineM(Foo.Baz)(fooMonoid)

    */
  extension[A] (a1: A) {
    // in Klammernpaar alles using oder nix using
    def combineM(a2: A)(using monoid: Monoid[A]): A = monoid.combine(a1, a2)
  }

  val foo1 = Foo.Bar.combineM(Foo.Baz)

  /*
  def combineAll[A](seq: Seq[A])(using monoid: Monoid[A]): A =
    seq.fold(monoid.neutral) { (a1, a2) => a1.combineM(a2) }
    // seq.fold(monoid.neutral)(monoid.combine)
  */
  // dafür syntaktischer Zucker
  def combineAll[A : Monoid](seq: Seq[A]): A =
    seq.fold(summon[Monoid[A]].neutral) { (a1, a2) => a1.combineM(a2) }


  given optionMonoid[A](using monoid: Monoid[A]): Monoid[Option[A]] =
    new Monoid[Option[A]] {
      override val neutral = None
      override def combine(o1: Option[A], o2: Option[A]): Option[A] =
        (o1, o2) match {
          case (None, _) => o2
          case (_, None) => o1
          case (Some(a1), Some(a2)) => Some(a1.combineM(a2))
        }
    }

  val op1: Option[Foo] = Some(Foo.Bar)
  val op2: Option[Foo] = Some(Foo.Baz)
  val o = op1.combineM(op2)

}