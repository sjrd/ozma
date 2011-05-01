package scala

package object ozma {
  @native def newUnbound[A]: A

  @native def thread[A](stat: => A): A
}
