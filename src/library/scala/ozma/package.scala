package scala

package object ozma {
  @native def newUnbound[A]: A = sys.error("stub")

  @native def thread[A](stat: => A): A = sys.error("stub")

  def whileLoop[U](cond: => Boolean)(body: => U) {
    if (cond) {
      body
      whileLoop(cond)(body)
    }
  }

  def doWhileLoop[U](body: => U)(cond: => Boolean) {
    body
    if (cond)
      doWhileLoop(body)(cond)
  }
}
