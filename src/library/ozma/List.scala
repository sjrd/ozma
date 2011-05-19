/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package ozma

import annotation.tailrec
import scala.ozma.tailcall

sealed abstract class List[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: List[A]

  // New methods in List

  def length = {
    def loop(list: List[A], acc: Int): Int = list match {
      case Nil => acc
      case _ :: tail => loop(tail, acc+1)
    }

    loop(this, 0)
  }

  def apply(index: Int): A = this match {
    case Nil => throw new java.lang.IndexOutOfBoundsException()
    case head :: tail => if (index == 0) head else tail.apply(index-1)
  }

  /** Adds an element at the beginning of this list.
   *  @param x the element to prepend.
   *  @return  a list which contains `x` as first element and
   *           which continues with this list.
   *  @example `1 :: List(2, 3) = List(2, 3).::(1) = List(1, 2, 3)`
   *  @usecase def ::(x: A): List[A]
   */
  def ::[B >: A] (x: B): List[B] =
    new ::(x, this)

  /** Adds the elements of a given list in front of this list.
   *  @param prefix  The list elements to prepend.
   *  @return a list resulting from the concatenation of the given
   *    list `prefix` and this list.
   *  @example `List(1, 2) ::: List(3, 4) = List(3, 4).:::(List(1, 2)) = List(1, 2, 3, 4)`
   *  @usecase def :::(prefix: List[A]): List[A]
   */
  def :::[B >: A](prefix: List[B]): List[B] = prefix match {
    case Nil => this
    case head :: tail => head :: (tail ::: this)
  }

  def foreach[U](f: A => U) {
    this match {
      case Nil => ()
      case head :: tail =>
        f(head)
        tail.foreach(f)
    }
  }

  def contains(x: Any): Boolean = this match {
    case Nil => false
    case head :: tail => if (x == head) true else tail.contains(x)
  }

  def filter(predicate: A => Boolean): List[A] = this match {
    case Nil => Nil
    case head :: tail =>
      if (predicate(head))
        head :: tail.filter(predicate)
      else
        tail.filter(predicate)
  }

  def take(n: Int): List[A] = {
    if (n == 0) Nil else {
      this match {
        case Nil => this
        case head :: tail => head :: tail.take(n-1)
      }
    }
  }

  def drop(n: Int): List[A] = {
    if (n == 0) this else {
      this match {
        case Nil => this
        case _ :: tail => tail.drop(n-1)
      }
    }
  }

  def reverse: List[A] = {
    def loop(list: List[A], acc: List[A]): List[A] = list match {
      case Nil => acc
      case head :: tail => loop(tail, head :: acc)
    }

    loop(this, Nil)
  }

  def map[B](f: A => B): List[B] = this match {
    case Nil => Nil
    case head :: tail => f(head) :: tail.map(f)
  }

  def foldLeft[B](init: B)(f: (B, A) => B): B = this match {
    case Nil => init
    case head :: tail => tail.foldLeft(f(init, head))(f)
  }

  def foldRight[B](init: B)(f: (A, B) => B): B = this match {
    case Nil => init
    case head :: tail => f(head, tail.foldRight(init)(f))
  }
}

object List {
}

/** The empty list.
 */
case object Nil extends List[Nothing] {
  override def isEmpty = true

  override def head: Nothing =
    throw new NoSuchElementException("head of empty list")

  override def tail: List[Nothing] =
    throw new UnsupportedOperationException("tail of empty list")
}

/** A non empty list characterized by a head and a tail.
 */
final case class ::[B](@tailcall val head: B, @tailcall val tail: List[B]) extends List[B] {
  override def isEmpty: Boolean = false
}
