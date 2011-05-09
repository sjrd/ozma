/**
 * This program expects the following input:
 *   ozma Calculator <float1> +|-|*|/ <float2>
 * Il will compute and display the result.
 *
 * Note that it is buggy on purpose: if less than three parameters are given,
 * it will "crash" with an ArrayIndexOutOfBoundException.
 *
 * If it is given a wrong float number, it will display a nice error message.
 * It will also do the same if given a wrong operator.
 */

object Calculator {
  def main(args: Array[String]) {
    try {
      val left = java.lang.Float.parseFloat(args(0))
      val op = args(1)
      val right = java.lang.Float.parseFloat(args(2))

      val result = op match {
        case "+" => left + right
        case "-" => left - right
        case "*" => left * right
        case "/" => left / right
      }

      Console.println(result)
    } catch {
      case error: NumberFormatException =>
        Console.println("The arguments must be floats")
        Console.println(error)
      case _: MatchError =>
        Console.println("Allowed operators are + - * /")
    }
  }
}
