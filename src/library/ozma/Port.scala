package ozma

import scala.ozma._

class Port[-A] private (private val rawPort: Any) {
  @native def send(element: A): Unit = sys.error("stub")
}

object Port {
  @native def newPort[A]: (List[A], Port[A]) = sys.error("stub")

  def make[A](handler: List[A] => Unit) = {
    val (stream, port) = newPort[A]
    thread {
      handler(stream)
    }
    port
  }

  def newStatelessPortObject[A, U](handler: A => U) =
    make[A](_ foreach handler)

  def newPortObject[A, B](init: B)(handler: (B, A) => B) =
    make[A](_.foldLeft(init)(handler))

  @native def newActiveObject[A <: AnyRef](obj: A): A = sys.error("stub")
}
