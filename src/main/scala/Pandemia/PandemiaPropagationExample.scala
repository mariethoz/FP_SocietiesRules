package Pandemia

import jdk.jfr.Threshold

import scala.collection.MapView
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

val observation_radius: Int = 5
val infection_radius: Float = 3
val virus_infection_chance: Float = 0.7

case class Person(id: Int, mind_status: BehaviorStatus = Neutral, health_status: HealthStatus = Healthy, x: Int, y: Int, mind_score: Int = 50, recovery_time: Int = 7) {
  def get_transmission_chance: Float = (health_status, mind_status) match {
    case (Infected, Comply) => 0.2
    case (Infected, Neutral) => 0.8
    case (Infected, Reject) => 1.0
    case _ => 0.0
  }

  def get_personal_health_modifier: Int = health_status match {
    case Healthy => -1
    case Infected => 3
    case Recovered => -2
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

def updateMindset(pop: Vector[Person],
                  thresholdComply: Int = 10,
                  thresholdReject: Int = -10,
                  thresholdNeutralUpper: Int = 5,
                  thresholdNeutralLower: Int = -5): Vector[Person] = {
  pop.map { p =>
    val newMindStatus = p.mind_score match {
      case score if score >= thresholdComply =>
        Comply
      case score if score <= thresholdReject =>
        Reject
      case score if score > thresholdNeutralLower && score < thresholdNeutralUpper =>
        Neutral
      case _ => p.mind_status // Retain current status if in overlapping zone
    }
    p.copy(mind_status = newMindStatus)
  }
}


def observation(pop: Vector[Person], radius: Int, considerHealth: Boolean = false): Vector[Person] = {
  val neighborsWithinRadius: Map[Int, Vector[Person]] =
    pop.map(p => p.id -> pop.filter(o => o.id != p.id && distance(p, o) <= radius)).toMap

  // Define the payoff matrix for the mind state interaction as a function
  // Each branch computes a tuple (totalReward, totalCost) based on neighbor counts.
  def calculatePayoff(p: Person, nearComply: Int, nearNeutral: Int, nearReject: Int,
                       nearHealthy: Int, nearInfected: Int, nearRecovered: Int): Int = {
    p.mind_status match {
    case Comply =>
      // Comply individuals:
      // - Reward when people adhere to the rules (nearComply).
      // - Some positive bonus for neutrals (they’re mildly proud).
      // - Cost when they see rejecting.
      if (!considerHealth) {
        nearComply * 2 + nearNeutral * 1 - nearReject * 5
      } else {
        // - As they saw infection they fear infection
        // - They know that recovered people do not transmit disease
        nearComply * 2 + nearNeutral * 1 - nearReject * 5 + nearInfected * 2 - nearRecovered
      }

    case Neutral =>
      // Neutral individuals:
      // - Their behavior is driven by the majority in their vicinity.
      //   We determine the majority and reward them accordingly.
      // - Their cost is derived from the conflicting minority influences.
      val majorityCount = Math.max(nearComply, Math.max(nearNeutral, nearReject))
      val healthCost: Int =
        if (!considerHealth) {
          0
        } else {
          // - As they saw infection they fear infection
          // - They know that recovered people do not transmit disease
          nearInfected * 2 - nearRecovered * 2
        }
      if (majorityCount == nearComply) {
        // Predominantly compliant environment: moderate reward from compliance influence.
        nearComply * 7 - nearNeutral * 5 + healthCost
      } else if (majorityCount == nearReject) {
        // Predominantly rejecting: they might feel the rebellious pull.
        nearReject * -7 + nearNeutral * 5
      } else {
        0 + healthCost
      }

    case Reject =>
      // Reject individuals:
      // - Inspired by compliant neighbors gives them a reward.
      // - However, many reject neighbors act as a reminder force if around comply.
      // - Neutral neighbors reinforce  their behavior.
      val healthCost: Int =
        if (!considerHealth) {
          0
        } else {
          // - They may not fear infection if many people are healthy
          // - Even through, the infection is reminded
          // - They know that recovered people do not transmit disease
          nearInfected * 2 - nearHealthy - nearRecovered * 2
        }
      if (nearComply > 0)
        nearComply * 5 + nearReject * 2 - nearNeutral * 5
      else
        - nearReject * 2 - nearNeutral * 5
    }
  }

  pop.map { p =>
    val buckets: Map[(Int, Int), Vector[Person]] =
      pop.groupBy(p => (p.x / radius, p.y / radius))
    val (bx, by) = (p.x / radius, p.y / radius)
    val candidates = for {
      dx <- -1 to 1
      dy <- -1 to 1
      neighbor <- buckets.getOrElse((bx + dx, by + dy), Vector.empty)
      if neighbor.id != p.id && distance(p, neighbor) <= radius
    } yield neighbor

    val mindCounts = candidates.groupBy(_.mind_status).view.mapValues(_.size)
    val healthCounts = candidates.groupBy(_.health_status).view.mapValues(_.size)


    val nearReject = mindCounts.getOrElse(Reject, 0)
    val nearNeutral = mindCounts.getOrElse(Neutral, 0)
    val nearComply = mindCounts.getOrElse(Comply, 0)

    // (Optional) Use health counts later or to further modify the score
    val nearInfected = healthCounts.getOrElse(Infected, 0)
    val nearHealthy = healthCounts.getOrElse(Healthy, 0)
    val nearRecovered = healthCounts.getOrElse(Recovered, 0)

    // Calculate the reward and cost from all interactions.
    p.copy(mind_score = calculatePayoff(p, nearComply, nearNeutral, nearReject,
      nearHealthy, nearInfected, nearRecovered) + p.get_personal_health_modifier )
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
  val infectedPeople = population.filter(_.health_status == Infected) // Pre-filter infected persons
  population.map { p =>
    p.health_status match {
      case Healthy =>
        // Find nearby infected individuals
        val nearbyInfected = infectedPeople.filter(o => distance(p, o) <= infection_radius)
        // Determine infection probability
        val willBeInfected = nearbyInfected.foldLeft(false) { (acc, neighbour) =>
          acc || math.random < (base_infection_chance * neighbour.get_transmission_chance)
        }
        if (willBeInfected) p.copy(health_status = Infected) else p
      case Infected =>
        // Manage recovery
        p.copy(health_status = if (p.recovery_time <= 1) Recovered else Infected, recovery_time = p.recovery_time - 1)
      case Recovered =>
        if (p.recovery_time <= -21)
          p.copy(health_status = Healthy, recovery_time = 7)
        else
          p.copy(recovery_time = p.recovery_time - 1)
      case _ => p
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
    val pop_mind = updateMindset(pop_observed)
    loop(pop_mind, step + 1)
  }

  loop(persons, 1)
}

def populationVector(size: Int, areaSize: Int, numComply: Int = 0, numReject: Int = 0, initiallyInfected: Int = 1): Vector[Person] = {
  if (numComply + numReject > size) Vector.empty[Person]
  val numNeutral = size - numComply - numReject

  // Shuffle indices to randomize mind status assignments
  val shuffledIndices = Random.shuffle((0 until size).toList)

  val complySet = shuffledIndices.take(numComply).toSet
  val rejectSet = shuffledIndices.slice(numComply, numComply + numReject).toSet
  val infectedSet = Random.shuffle(shuffledIndices).take(initiallyInfected).toSet

  Vector.tabulate(size) { i =>
    val healthStatus = if (infectedSet.contains(i)) Infected else Healthy
    val mindScore = if (complySet.contains(i)) 45
    else if (rejectSet.contains(i)) -45
    else 0

    Person(
      id = i,
      mind_status = Neutral,
      health_status = healthStatus,
      x = (math.random() * areaSize).toInt,
      y = (math.random() * areaSize).toInt,
      mind_score = mindScore
    )
  }
}


object PandemiaPropagationExample {
  def main(args: Array[String]): Unit = {
    val areaSize = 10
    var population = populationVector(11, areaSize)
    population = updateMindset(population)
    println(population)
    simulate(population, areaSize, 10)
  }
}
