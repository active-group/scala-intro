object  Animals {

  type Weight = Double

  enum Liveness {
    case Alive
    case Dead
  }

  enum Animal {
    case Dillo(liveness: Liveness, weight: Weight)
    case Parrot(sentence: String, weight: Weight)

    // Konvention: rein funktionale Methoden ohne Argumente ohne Klammern
    def runOver: Animal =
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

  // val: Wert, def: Funktion

  def runOver(animal: Animal): Animal = 
    animal match {
      case dillo@Dillo(liveness, weight) =>
        // Dillo(Liveness.Dead, weight)
        dillo.copy(liveness = Liveness.Dead)
      case Parrot(sentence, weight) =>
        Parrot("", weight)
    }

  val dillo1RunOver = runOver.apply(dillo1)
  val dillo1RunOver2 = runOver(dillo1) // = dillo1RunOver
  val dillo1RunOver3 = dillo1.runOver
  import language.postfixOps
  val dillo1RunOver4 = dillo1 runOver // = dillo1RunOver3 

  val highway = List(dillo1, dillo2)

  def runOverAnimals(animals: List[Animal]): List[Animal] =
    animals match {
      case Nil => ???
      case head :: next => ???
    }

}