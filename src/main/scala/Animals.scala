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

  val dillo = Animal.Dillo(Liveness.Alive, 10)

}