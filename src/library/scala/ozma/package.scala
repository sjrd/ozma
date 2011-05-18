package scala

package object ozma {
  @native def newUnbound[A]: A = sys.error("stub")

  @native def thread[A](stat: => A): A = sys.error("stub")

  @native def waitBound(value: Any): Unit = sys.error("stub")
  @native def waitNeeded(value: Any): Unit = sys.error("stub")

  @native def byNeed[A](value: => A): A = sys.error("stub")
  @native def byNeedFuture[A](value: => A): A = sys.error("stub")

  @native def sleep(ms: Int): Unit = sys.error("stub")

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
