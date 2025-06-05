package Behavior

import scala.util.Random
import scala.util.Random.nextInt

sealed trait BehaviorStatus
case object Comply extends BehaviorStatus // Mind >= 75
case object Neutral extends BehaviorStatus   // 75 > Mind >= 40
case object Reject extends BehaviorStatus    // 40 > Mind >= 0

case class Person(id: Int, status: BehaviorStatus, x: Int, y: Int, mindScore: Int = 50)

object AdoptedBehavior {


  def maxDistance(p: Person, newX: Int, newY: Int, pop: Vector[Person]): Option[Double] = {
    pop.filter(_.id != p.id) // Exclude p itself
      .map(other => math.hypot(newX - other.x, newY - other.y)) // Compute distances
      .minOption
  }


  def move(person: Person, areaSize: Int, pop: Vector[Person]): Person = {

    val possibleMoves = for {
      dx <- -1 to 1
      dy <- -1 to 1
      newX = (person.x + dx).max(0).min(areaSize - 1)
      newY = (person.y + dy).max(0).min(areaSize - 1)
    } yield (newX, newY, maxDistance(person, newX, newY, pop))


    // Apply the best move
    person.status match {
      case Comply => {
        val filteredMoves = possibleMoves.collect { case (x, y, Some(dist)) if dist > 3.0 => (x, y, dist) }
        if (filteredMoves.nonEmpty) {
          val (nx, ny, _) = filteredMoves(Random.nextInt(filteredMoves.size))
          person.copy(x = nx, y = ny)
        } else person // No valid move, stay in place
        /** To select the max distance instead of the distance
        val bestMove = possibleMoves
          .collect { case (x, y, Some(maxDist)) => (x, y, maxDist) } // Remove None cases
          .sortBy(-_._3) // Sort by min distance
          .headOption // Safely take the best move
        bestMove match {
          case Some((nx, ny, _)) => person.copy(x = nx, y = ny)
          case None => person // No valid move, stay in place
        }
         */
      }
      case Neutral => {
        // Simulate a moderate compliance of the distance rule
        val filteredMoves = possibleMoves.collect { case (x, y, Some(dist)) if dist > 1 => (x, y, dist) }
        if (filteredMoves.nonEmpty) {
          val (nx, ny, _) = filteredMoves(Random.nextInt(filteredMoves.size))
          person.copy(x = nx, y = ny)
        } else person // No valid move, stay in place
      }
      case Reject => {
        val (nx, ny, _) = possibleMoves(Random.nextInt(possibleMoves.size))
        person.copy(x = nx, y = ny)
      }
    }
  }


  def distance(p1: Person, p2: Person): Double = {
    math.hypot(p1.x - p2.x, p1.y - p2.y)
  }

  def initPerson(pop: Vector[Person]): Vector[Person] = {
    pop.map { p =>
      if (p.mindScore < 40) p.copy(status = Reject)
      else if (p.mindScore < 75) p.copy(status = Neutral)
      else p.copy(status = Comply)
    }
  }

  def mindset(pop: Vector[Person]): Vector[Person] = {
    pop.map { p =>
      p.status match {
        case Comply =>
          if (p.mindScore < 40) p.copy(status = Reject)
          else if (p.mindScore < 75) p.copy(status = Neutral)
          else p

        case Neutral =>
          if (p.mindScore >= 75) p.copy(status = Comply)
          else if (p.mindScore < 40) p.copy(status = Reject)
          else p

        case Reject =>
          if (p.mindScore >= 75) p.copy(status = Comply)
          else if (p.mindScore >= 40) p.copy(status = Neutral)
          else p
      }
    }
  }

  def observation(pop: Vector[Person], radius: Int): Vector[Person] = {
    pop.map { p =>
      p.status match {
        case Comply =>
          // Count all Reject individuals within radius * 2 and Neutral within radius, excluding self.
          val nearReject = pop.count(o => o.id != p.id && o.status == Reject && distance(p, o) <= radius * 2)
          val nearNeutral = pop.count(o => o.id != p.id && o.status == Neutral && distance(p, o) <= radius)
          p.copy(mindScore = (p.mindScore - (nearReject * 3) - (nearNeutral * 1)).max(0).min(100))
        case Neutral =>
          val nearReject = pop.count(o => o.id != p.id && o.status == Reject && distance(p, o) <= radius)
          val nearComply = pop.count(o => o.id != p.id && o.status == Comply && distance(p, o) <= radius)
          p.copy(mindScore = (p.mindScore - (nearReject * 2) + (nearComply * 2)).max(0).min(100))
        case Reject =>
          val nearNeutral = pop.count(o => o.id != p.id && o.status == Neutral && distance(p, o) <= radius)
          val nearComply = pop.count(o => o.id != p.id && o.status == Comply && distance(p, o) <= radius * 2)
          p.copy(mindScore = (p.mindScore + (nearNeutral * 1) + (nearComply * 3)).max(0).min(100))
      }
    }
  }

  val symbol: Map[BehaviorStatus, Char] = Map(
    Neutral -> '.',
    Comply -> 'C',
    Reject  -> 'R'
  )

  def displayPopulation(pop: Vector[Person], areaSize: Int): Unit = {
    val grid = Array.fill(areaSize, areaSize)(' ')
    pop.foreach { p =>
      grid(p.y)(p.x) = symbol(p.status)
    }
    grid.foreach(row => println(row.mkString(" ")))
  }

  def simulate(population: Vector[Person], areaSize: Int, steps: Int): Unit = {
    def loop(pop: Vector[Person], step: Int): Unit = {
      if (step == 0) return
      println(s"Step $step:")
      displayPopulation(pop, areaSize)
      println(pop)

      val moved = pop.map(p => move(p, areaSize, pop))
      val updatedMindset = mindset(moved)
      val observed = observation(updatedMindset, radius = 2)
      loop(observed, step - 1)
    }
    loop(population, steps)
  }

  def populationVector(size: Int, areaSize: Int): Vector[Person] = {
    Vector.tabulate(size) { i =>
      Person(i, Neutral, (math.random() * areaSize).toInt, (math.random() * areaSize).toInt, (math.random() * 100).toInt)
    }
  }

  def main(args: Array[String]): Unit = {
    val areaSize = 10
    var population = populationVector(11, areaSize)
    println("Initial Population:")
    population = initPerson(population)
    println(population)

    simulate(population, areaSize, steps = 10)
  }
}
