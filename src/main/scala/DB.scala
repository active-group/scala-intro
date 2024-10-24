import java.io.ObjectOutputStream.PutField
object DB {
    type Key = String
    type Value = Int

    enum DB[A] {
      case Get(key: Key, callback: Value => DB[A])
      case Put(key: Key, value: Value, callback: Unit => DB[A])
      case Return(result: A)
      def flatMap[B](next: A => DB[B]): DB[B] =
        this match {
          case Get(key, callback) =>
            Get(key, { value => callback(value).flatMap(next) })
          case Put(key, value, callback) =>
            Put(key, value, { unit => callback(()).flatMap(next)})
          case Return(result) => next(result)
        }
      def map[B](f: A => B): DB[B] =
        this match {
          case Get(key, callback) =>
            Get(key, { value => callback(value).map(f)})
          case Put(key, value, callback) =>
            Put(key, value, { unit => callback(()).map(f)})
          case Return(result) => Return(f(result))
        }
    }

    import DB._
    def put(key: Key, value: Value): DB[Unit] = Put(key, value, Return.apply)
    def get(key: Key): DB[Value] = Get(key, Return.apply)


    val p1 = for {
      _ <- put("Mike", 100)
      x <- get("Mike")
      _ <- put("Mike", x+1)
      y <- get("Mike")
    } yield x+y

    val p1f = put("Mike", 100).flatMap { _ =>
              get("Mike").flatMap { x => 
              put("Mike", x+1).flatMap { _ =>
              get("Mike").map { y =>
              x+y}}}}
 
}