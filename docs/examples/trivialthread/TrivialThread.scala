import scala.ozma.thread

object TrivialThread {
  def main(args: Array[String]) {
    Console.println("Start program")
    val result = thread(fibonacci(35))
    Console.println("Continuing main method while computation is running")
    Console.println("The next statement will block because 'result' is needed")
    Console.println("Fib(35) = " + result)
  }

  def fibonacci(arg: Int): Int = arg match {
    case 0 | 1 => 1
    case _ => fibonacci(arg-1) + fibonacci(arg-2)
  }
}
