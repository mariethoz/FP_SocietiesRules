object FunctionalExample {
  def main(args: Array[String]): Unit = {
    val numbers = List(1, 2, 3, 4, 5)

    val doubled = numbers.map(_ * 2)

    val evenOnly = numbers.filter(_ % 2 == 0)

    val sum = numbers.reduce(_ + _)

    println(s"Doubled: $doubled")
    println(s"Even: $evenOnly")
    println(s"Sum: $sum")
  }
}