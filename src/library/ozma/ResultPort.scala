package ozma

import scala.ozma._

class ResultPort[-A, +R] private (private val rawPort: Any) {
  @native def send(element: A): R = sys.error("stub")
}

object ResultPort {
  @native def newPort[A, R]: (List[Item[A, R]], ResultPort[A, R]) = sys.error("stub")

  def make[A, R](handler: List[Item[A, R]] => Unit) = {
    val (stream, port) = newPort[A, R]
    threadGC(stream)(handler)
    port
  }

  def newStatelessPortObject[A, R](handler: A => R) =
    make[A, R](_.toAgent foreach {
      item => item.output = handler(item.input)
    })

  def newPortObject[A, B, R](init: B)(handler: (B, A) => (R, B)) =
    make[A, R](_.toAgent.foldLeft(init) { (prevState, item) =>
      val (result, newState) = handler(prevState, item.input)
      item.output = result
      newState
    })

  @native def newActiveObject[A <: AnyRef](obj: A): A = sys.error("stub")

  class Item[+A, R] private (val input: A, _output: R) {
    @singleAssignment var output = _output
  }
}
