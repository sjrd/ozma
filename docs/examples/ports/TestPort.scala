import ozma._
import scala.ozma._

object TestPort {
  def main(args: Array[String]) {
    // Create a port whose handler will display every arriving element
    val port = Port.newStatelessPortObject(Console.println)

    // Now 3 threads that send different ranges of integers to the port
    val X1: Unit
    val X2: Unit
    val X3: Unit

    thread { generator(port, 1, 10); X1 = () }
    thread { generator(port, 11, 20); X2 = X1 }
    thread { generator(port, 21, 30); X3 = X2 }

    waitBound(X3)
  }

  /**
   * Generate the integers between 'from' and 'to' included, and send them in
   * ascending order into the given 'port'
   */
  def generator(port: Port[Int], from: Int, to: Int) {
    if (from <= to) {
      port.send(from)
      sleep(300) // delay to be able to observe the interleaving of threads
      generator(port, from + 1, to)
    }
  }
}
