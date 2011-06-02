abstract class Tree[+A]
case object Leaf extends Tree[Nothing]
case class Node[+A](value: A, left: Tree[A] = Leaf,
    right: Tree[A] = Leaf) extends Tree[A]

object BinaryTrees {
  type Comparer[-A] = (A, A) => Boolean

  def insert[A](smaller: Comparer[A])(tree: Tree[A], value: A): Tree[A] = tree match {
    case Leaf => Node(value, Leaf, Leaf)

    case Node(v, left, right) =>
      if (smaller(value, v))
        Node(v, insert(smaller)(left, value), right)
      else
        Node(v, left, insert(smaller)(right, value))
  }

  def displayTree(tree: Tree[Any]) {
    tree match {
      case Leaf => ()
      case Node(value, left, right) =>
        displayTree(left)
        println(value)
        displayTree(right)
    }
  }

  def insertMany[A](smaller: Comparer[A])(tree: Tree[A], values: TraversableOnce[A]) =
    values.foldLeft(tree)(insert(smaller))

  def main(args: Array[String]) {
    val tree = insertMany((_:Int) < (_:Int))(Leaf, List(5, 1, 9, 8, 9, 6, 2, 10))
    displayTree(tree)
  }
}
