package scala

package object ozma {
  @native def newUnbound[A]: A = sys.error("stub")

  @native def thread[A](stat: => A): A = sys.error("stub")
}
