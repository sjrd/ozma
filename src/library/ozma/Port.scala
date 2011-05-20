package ozma

import scala.ozma._

class Port[-A](private val sendProc: A => Unit) {
  def send(element: A) = sendProc(element)
}

object Port {
  def make[A](handler: List[A] => Unit) = {
    // must be parsable by Scala
    @singleAssignment var head: List[A] = newUnbound

    thread {
      handler(head)
    }

    new Port(new SendProc(head))
  }

  def newPortObject[A, U](handler: A => U) =
    make[A](_ foreach handler)

  def newPortObject[A, B](init: B)(handler: (B, A) => B) =
    make[A](_.foldLeft(init)(handler))

  @native def newActiveObject[A <: AnyRef](obj: A): A = sys.error("stub")

  private class SendProc[A](head: List[A]) extends Function1[A, Unit] {
    private[this] var tail = head

    def apply(element: A) {
      @singleAssignment var newTail: List[A] = newUnbound
      val oldTail = element :: newTail
      newTail = putOldAndGetNewTail(oldTail)
    }

    @native def putOldAndGetNewTail(old: List[A]): List[A] = sys.error("stub")
  }
}
