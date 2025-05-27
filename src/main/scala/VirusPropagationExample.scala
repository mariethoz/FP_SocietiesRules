import scala.collection.mutable.ArrayBuffer
import scala.util.Random.nextInt

class VirusPropagationExample

sealed trait HealthStatus
case object Healthy extends HealthStatus
case object Infected extends HealthStatus
case object Recovered extends HealthStatus

case class Person(id: Int, status: HealthStatus, x: Int, y: Int, recovery_time: Int = 7)

def move(person: Person, areaSize: Int): Person = {
  val dx = nextInt(3) - 1 // -1, 0, or 1
  val dy = nextInt(3) - 1
  val newX = (person.x + dx).max(0).min(areaSize - 1)
  val newY = (person.y + dy).max(0).min(areaSize - 1)
  person.copy(x = newX, y = newY)
}

def distance(p1: Person, p2: Person): Double = {
  math.sqrt(math.pow(p1.x - p2.x, 2) + math.pow(p1.y - p2.y, 2))
}


def infect(population: Vector[Person], infectionRadius: Double, infectionChance: Double): Vector[Person] = {
  population.map { p =>
    if (p.status == Healthy) {
      val nearInfected = population.exists { other =>
        other.status == Infected && distance(p, other) <= infectionRadius
      }
      if (nearInfected && math.random < infectionChance)
        p.copy(status = Infected)
      else p
    } else if (p.status == Infected) {
      if (p.recovery_time-1 == 0) {p.copy(status = Recovered)} else {p.copy(recovery_time = p.recovery_time-1)}
    } else p
  }
}


val symbol: Map[HealthStatus, Char] = Map(
  Healthy -> '.',
  Infected -> 'X',
  Recovered -> 'R'
)

def displayPopulation(pop: Vector[Person], areaSize: Int): Unit = {
  val grid = Array.fill(areaSize, areaSize)(' ')

  for (p <- pop) {
    val symbol = p.status match {
      case Healthy => '.'
      case Infected => 'X'
      case Recovered => 'R'
    }
    grid(p.y)(p.x) = symbol
  }

  grid.foreach(row => println(row.mkString(" ")))
}


def simulate(population: Vector[Person], areaSize: Int, steps: Int): Unit = {
  def loop(pop: Vector[Person], step: Int): Unit = {
    if (step > steps) return
    println(s"Step $step:")
    displayPopulation(pop, areaSize)

    val moved = pop.map(p => move(p, areaSize))
    val infected = infect(moved, infectionRadius = 2.0, infectionChance = 0.5)
    loop(infected, step + 1)
  }

  loop(population, 1)
}

object VirusPropagationExample {
  def main(args: Array[String]): Unit = {
    val areaSize = 10
    var population = Vector[Person](Person(0, Infected, (math.random() * areaSize).toInt, (math.random() * areaSize).toInt))
    for (i <- 1 to 10) {
      population = population :+ Person(i, Healthy, (math.random() * areaSize).toInt, (math.random() * areaSize).toInt)
    }
    println(population)
    simulate(population, areaSize, 10)
  }
}
