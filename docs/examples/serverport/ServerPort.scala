import scala.ozma._
import ozma._

object ServerPort {
  def makeSecondDegreeServer(a: Int, b: Int, c: Int) = {
    ResultPort.newStatelessPortObject {
      x: Int => a*x*x + b*x + c
    }
  }

  def main(args: Array[String]) {
    val ports = List(
        makeSecondDegreeServer(3, 2, -4),
        makeSecondDegreeServer(1, 2, 3),
        makeSecondDegreeServer(5, -4, -4),
        makeSecondDegreeServer(0, 0, 3),
        makeSecondDegreeServer(3, 2, 1),
        makeSecondDegreeServer(-1, -1, -1)
    )

    val results = ports map (_ send 3)
    results foreach println

    println("max: " + results.max)
  }
}
