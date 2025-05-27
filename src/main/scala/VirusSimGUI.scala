import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.{Button, Label, Slider}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.paint.Color
import scalafx.animation.AnimationTimer
import scalafx.geometry.Insets

import scala.util.Random

object VirusSimGUI extends JFXApp3 {

  val areaSize = 20
  val cellSize = 20
  val canvasWidth = areaSize * cellSize
  val canvasHeight = areaSize * cellSize

  var population: Vector[Person] = Vector(Person(0, Infected, Random.nextInt(areaSize), Random.nextInt(areaSize))) ++
    (1 to 50).map(i => Person(i, Healthy, Random.nextInt(areaSize), Random.nextInt(areaSize)))

  def move(p: Person): Person = {
    val dx = Random.nextInt(5) - 2
    val dy = Random.nextInt(5) - 2
    val newX = (p.x + dx).max(0).min(areaSize - 1)
    val newY = (p.y + dy).max(0).min(areaSize - 1)
    p.copy(x = newX, y = newY)
  }

  def distance(p1: Person, p2: Person): Double =
    math.hypot(p1.x - p2.x, p1.y - p2.y)

  def infect(pop: Vector[Person], radius: Double, chance: Double): Vector[Person] = {
    pop.map { p =>
      p.status match {
        case Healthy =>
          val nearInfected = pop.exists(o => o.status == Infected && distance(p, o) <= radius)
          if (nearInfected && math.random < chance) p.copy(status = Infected)
          else p
        case Infected =>
          if (p.recovery_time <= 1) p.copy(status = Recovered)
          else p.copy(recovery_time = p.recovery_time - 1)
        case _ => p
      }
    }
  }


  var isRunning = false
  var lastUpdateTime: Long = 0L

  override def start(): Unit = {

    val canvas = new Canvas(canvasWidth, canvasHeight)
    val gc = canvas.graphicsContext2D

    val labelHealthy = new Label("Healthy: 0")
    val labelInfected = new Label("Infected: 0")
    val labelRecovered = new Label("Recovered: 0")

    val sliderSpeed = new Slider(100, 1000, 200) {
      showTickLabels = true
      showTickMarks = true
      majorTickUnit = 300
      minorTickCount = 2
      blockIncrement = 100
    }

    val buttonToggle = new Button("Start")

    buttonToggle.onAction = _ => {
      isRunning = !isRunning
      buttonToggle.text = if (isRunning) "Pause" else "Resume"
    }

    val layout = new VBox(10) {
      padding = Insets(10)
      children = Seq(
        canvas,
        new HBox(10, labelHealthy, labelInfected, labelRecovered),
        new HBox(10, new Label("Speed (ms):"), sliderSpeed, buttonToggle)
      )
    }

    stage = new JFXApp3.PrimaryStage {
      title.value = "Virus Simulation"
      scene = new Scene(canvasWidth + 20, canvasHeight + 100) {
        content = layout
      }
    }

    def draw(): Unit = {
      gc.fill = Color.White
      gc.fillRect(0, 0, canvasWidth, canvasHeight)

      for (p <- population) {
        gc.fill = p.status match {
          case Healthy => Color.Green
          case Infected => Color.Red
          case Recovered => Color.Blue
        }
        gc.fillOval(p.x * cellSize, p.y * cellSize, cellSize - 2, cellSize - 2)
      }
    }

    def updateCounters(): Unit = {
      val healthy = population.count(_.status == Healthy)
      val infected = population.count(_.status == Infected)
      val recovered = population.count(_.status == Recovered)

      labelHealthy.text = s"Healthy: $healthy"
      labelInfected.text = s"Infected: $infected"
      labelRecovered.text = s"Recovered: $recovered"
      if (infected == 0) isRunning = false
    }

    val timer = AnimationTimer { now =>
      val delay = (sliderSpeed.value() * 1_000_000).toLong
      if (isRunning && now - lastUpdateTime > delay) {
        population = population.map(move)
        population = infect(population, radius = 2.0, chance = 0.5)
        draw()
        updateCounters()
        lastUpdateTime = now
      }
    }

    timer.start()
  }
}
