package Pandemia

import scala.util.Random
import scala.util.Random.nextInt

sealed trait BehaviorStatus
case object Comply extends BehaviorStatus // Mind >= 75
case object Neutral extends BehaviorStatus   // 75 > Mind >= 40
case object Reject extends BehaviorStatus    // 40 > Mind >= 0


sealed trait HealthStatus
case object Healthy extends HealthStatus
case object Infected extends HealthStatus
case object Recovered extends HealthStatus

val observation_radius: Float = 5
val infection_radius: Float = 3
val virus_infection_chance: Float = 0.7

case class Person(id: Int, mind_status: BehaviorStatus = Neutral, health_status: HealthStatus = Healthy, x: Int, y: Int, mind_score: Int = 50, recovery_time: Int = 7) {
  def get_transmission_chance(): Float = {
    if (health_status == Infected) {
      mind_status match {
        case Comply => 0.2
        case Neutral => 0.8
        case Reject => 1
      }
    } else 0
  }
}

def distance(p1: Person, p2: Person): Double = {
  math.hypot(p1.x - p2.x, p1.y - p2.y)
}
def behaviorFromScore(score: Int): BehaviorStatus = {
  if (score >= 75) Comply
  else if (score >= 40) Neutral
  else Reject
}

def update_mindset(pop: Vector[Person]): Vector[Person] = {
  pop.map(p => p.copy(mind_status = behaviorFromScore(p.mind_score)))
}

def clampMindScore(score: Int): Int = score.max(0).min(100)

def observation(pop: Vector[Person], radius: Float): Vector[Person] = {
  pop.map { p =>
    p.mind_status match {
      case Comply =>
        // Count all Reject individuals within radius * 2 and Neutral within radius, excluding self.
        val nearReject = pop.count(o => o.id != p.id && o.mind_status == Reject && distance(p, o) <= radius * 2)
        val nearNeutral = pop.count(o => o.id != p.id && o.mind_status == Neutral && distance(p, o) <= radius)
        val nearInfected = pop.count(o => o.id != p.id && o.health_status == Infected && distance(p, o) <= radius * 2)
        val nearHealthy = pop.count(o => o.id != p.id && o.health_status == Healthy && distance(p, o) <= radius * 2)
        val positive = nearInfected * 2
        val negative = nearHealthy * 1 + nearReject * 3 + nearNeutral * 1
        p.copy(mind_score = clampMindScore(p.mind_score + positive - negative))
      case Neutral =>
        val nearReject = pop.count(o => o.id != p.id && o.mind_status == Reject && distance(p, o) <= radius)
        val nearComply = pop.count(o => o.id != p.id && o.mind_status == Comply && distance(p, o) <= radius)
        val nearInfected = pop.count(o => o.id != p.id && o.health_status == Infected && distance(p, o) <= radius * 2)
        val nearHealthy = pop.count(o => o.id != p.id && o.health_status == Healthy && distance(p, o) <= radius * 2)
        val positive = nearInfected * 1 + nearComply * 2
        val negative = nearHealthy * 1 + nearReject * 2
        p.copy(mind_score = clampMindScore(p.mind_score + positive - negative))
      case Reject =>
        val nearNeutral = pop.count(o => o.id != p.id && o.mind_status == Neutral && distance(p, o) <= radius)
        val nearComply = pop.count(o => o.id != p.id && o.mind_status == Comply && distance(p, o) <= radius * 2)
        val nearInfected = pop.count(o => o.id != p.id && o.health_status == Infected && distance(p, o) <= radius * 2)
        val nearHealthy = pop.count(o => o.id != p.id && o.health_status == Healthy && distance(p, o) <= radius * 2)
        val positive = nearInfected * 1 + nearNeutral * 1 + nearComply * 3
        val negative = nearHealthy * 2
        p.copy(mind_score = clampMindScore(p.mind_score + positive - negative))
        p.copy(mind_score = clampMindScore(p.mind_score + (nearNeutral * 1) + (nearComply * 3)))
    }
  }
}

def minDistance(p: Person, newX: Int, newY: Int, pop: Vector[Person]): Option[Double] = {
  pop.filter(_.id != p.id) // Exclude p itself
    .map(other => math.hypot(newX - other.x, newY - other.y)) // Compute distances
    .minOption
}

def move(population: Vector[Person], areaSize: Int): Vector[Person] = {
  population.map { person =>
    // Generate potential moves from current position: all adjacent cells (including staying in place)
    // while ensuring moves stay within the area bounds.
    val possibleMoves = for {
      dx <- -1 to 1
      dy <- -1 to 1
      newX = (person.x + dx).max(0).min(areaSize - 1)
      newY = (person.y + dy).max(0).min(areaSize - 1)
      // Calculate the minimum distance from (newX, newY) to any other person.
      distOpt = minDistance(person, newX, newY, population)
    } yield (newX, newY, distOpt)

    person.mind_status match {
      case Comply =>
        // For those who comply, we first filter for moves where the minimum distance is above 3.0.
        val validMoves = possibleMoves.collect {
          case (x, y, Some(dist)) if dist > 3.0 => (x, y, dist)
        }
        if (validMoves.nonEmpty) {
          // Choose the move that maximizes the distance from others.
          val (nx, ny, _) = validMoves.maxBy(_._3)
          person.copy(x = nx, y = ny)
        } else {
          // If no move satisfies the threshold, the person stays in place.
          person
        }

      case Neutral =>
        // Neutral persons require a less strict distance, here more than 1.0.
        val validMoves = possibleMoves.collect {
          case (x, y, Some(dist)) if dist > 1.0 => (x, y, dist)
        }
        if (validMoves.nonEmpty) {
          // Again, choose the move with maximal distancing.
          val (nx, ny, _) = validMoves.maxBy(_._3)
          person.copy(x = nx, y = ny)
        } else {
          person
        }

      case Reject =>
        // For those rejecting the norm, make a random move from all the available possibilities.
        val (nx, ny, _) = possibleMoves(Random.nextInt(possibleMoves.size))
        person.copy(x = nx, y = ny)
    }
  }
}

def infect(population: Vector[Person], infection_radius: Float, base_infection_chance: Float): Vector[Person] = {
  population.map { p =>
    if (p.health_status == Healthy) {
      // Find all infected neighbors within the specified radius.
      val infectedNeighbours = population.filter { other =>
        other.health_status == Infected && distance(p, other) <= infection_radius
      }
      // Check if at least one neighbor successfully infects p.
      // Each neighbor's chance is modulated by its get_transmission_chance.
      val willBeInfected = infectedNeighbours.exists { neighbour =>
        math.random < (base_infection_chance * neighbour.get_transmission_chance())
      }
      if (willBeInfected) p.copy(health_status = Infected) else p
    } else if (p.health_status == Infected) {
      // Update recovery: if it's the last day, set to Recovered; otherwise decrement recovery_time.
      if (p.recovery_time <= 1)
        p.copy(health_status = Recovered)
      else
        p.copy(recovery_time = p.recovery_time - 1)
    } else {
      p
    }
  }
}

def displayPopulation(pop: Vector[Person], areaSize: Int): Unit = {
  val health_grid = Array.fill(areaSize, areaSize)(' ')
  val mind_grid = Array.fill(areaSize, areaSize)(' ')

  for (p <- pop) {
    val symbol = p.health_status match {
      case Healthy => '.'
      case Infected => 'X'
      case Recovered => 'R'
    }
    health_grid(p.y)(p.x) = symbol
  }
  for (p <- pop) {
    val symbol = p.mind_status match {
      case Comply => 'C'
      case Neutral => 'N'
      case Reject => 'R'
    }
    mind_grid(p.y)(p.x) = symbol
  }

  health_grid.foreach(row => println(row.mkString("_")))
  mind_grid.foreach(row => println(row.mkString("_")))
}

def simulate(persons: Vector[Person], areaSize: Int, steps: Int): Unit = {
  def loop(pop: Vector[Person], step: Int): Unit = {
    if (step > steps) return
    println(s"Step $step:")
    displayPopulation(pop, areaSize)

    val moved = move(pop, areaSize)
    val infected = infect(moved, infection_radius, virus_infection_chance)
    val pop_observed = observation(infected, observation_radius)
    val pop_mind = update_mindset(pop_observed)
    loop(pop_mind, step + 1)
  }

  loop(persons, 1)
}

def populationVector(size: Int, areaSize: Int): Vector[Person] = {
  Vector.tabulate(size) { i =>
    Person(i, Neutral, List(Infected, Healthy, Healthy)(nextInt(3)), (math.random() * areaSize).toInt, (math.random() * areaSize).toInt, (math.random() * 100).toInt)
  }
}

object PandemiaPropagationExample {
  def main(args: Array[String]): Unit = {
    val areaSize = 10
    var population = populationVector(11, areaSize)
    population = update_mindset(population)
    println(population)
    simulate(population, areaSize, 10)
  }
}
