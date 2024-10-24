object  Animals {

  type Weight = Double

  enum Liveness {
    case Alive
    case Dead
  }

  enum Animal {
    case Dillo(liveness: Liveness, weight: Weight)
    case Parrot(sentence: String, weight: Weight)

    def runOver(): Animal =
      this match {
        case dillo@Dillo(liveness, weight) =>
          // Dillo(Liveness.Dead, weight)
          dillo.copy(liveness = Liveness.Dead)
        case Parrot(sentence, weight) =>
          Parrot("", weight)
    }
  }

  val dillo1 = Animal.Dillo(Liveness.Alive, 10)
  val dillo2 = {
    import Animal._
    Dillo(Liveness.Dead, 8)
  }

  import Animal._

  def runOver(animal: Animal): Animal = 
    animal match {
      case dillo@Dillo(liveness, weight) =>
        // Dillo(Liveness.Dead, weight)
        dillo.copy(liveness = Liveness.Dead)
      case Parrot(sentence, weight) =>
        Parrot("", weight)
    }

}