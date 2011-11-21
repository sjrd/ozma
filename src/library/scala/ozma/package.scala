package scala

package object ozma {
  @native def newUnbound[@specialized A]: A = sys.error("stub")

  def thread[@specialized A](stat: A): A = sys.error("stub")

  @native def waitBound[@specialized A](value: A): Unit = sys.error("stub")
  @native def waitQuiet[@specialized A](value: A): Unit = sys.error("stub")
  @native def waitNeeded[@specialized A](value: A): Unit = sys.error("stub")

  @native def byNeed[@specialized A](value: => A): A = sys.error("stub")
  @native def byNeedFuture[@specialized A](value: => A): A = sys.error("stub")

  @native def makeFailedValue[@specialized A](throwable: Throwable): A =
    sys.error("stub")

  @native def sleep(ms: Int): Unit = sys.error("stub")

  def whileLoop(cond: => Boolean)(body: => Unit) {
    if (cond) {
      body
      whileLoop(cond)(body)
    }
  }

  def doWhileLoop(body: => Unit)(cond: => Boolean) {
    body
    if (cond)
      doWhileLoop(body)(cond)
  }

  implicit def wrapList[A](list: List[A]) = new ListWrapper(list)

  implicit def listUnwrapperFromAgent[A](agent: ListAgent[A]) = agent.toList
  implicit def lazyListToList[A](lazyList: LazyList[A]) = lazyList.toList
}
