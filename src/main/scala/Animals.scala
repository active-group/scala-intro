object  Animals {

  type Weight = Double

  enum Liveness {
    case Alive
    case Dead
  }

  enum Animal {
    case Dillo(liveness: Liveness, weight: Weight)
    case Parrot(sentence: String, weight: Weight)
  }

  val dillo1 = Animal.Dillo(Liveness.Alive, 10)
  val dillo2 = {
    import Animal._
    Dillo(Liveness.Dead, 8)
  }


}