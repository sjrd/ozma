object Echo {
  def main(args: Array[String]) {
    var i = 0
    while (i < args.length) {
      Console.println(args(i))
      i += 1
    }
  }
}
