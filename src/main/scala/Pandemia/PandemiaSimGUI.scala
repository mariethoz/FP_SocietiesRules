package Pandemia

import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp3
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.{Button, Label, Slider}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.paint.Color

object PandemiaSimGUI extends JFXApp3 {


  val areaSize: Int = 50
  val pop_size: Int = 150
  val canvasSize: Int = 800

  val observation_radius: Float = 5
  val infection_radius: Float = 3
  val virus_infection_chance: Float = 0.9

  var population: Vector[Person] = populationVector(pop_size, areaSize)

  var isRunning = false
  var lastUpdateTime: Long = 0L

  override def start(): Unit = {
    def scale(value: Int, maxLogical: Int, maxVisual: Double): Double =
      value.toDouble / maxLogical * maxVisual


    val canvas = new Canvas(canvasSize, canvasSize)
    val gc = canvas.graphicsContext2D

    val labelComply = new Label("Comply: 0")
    val labelNeutral = new Label("Neutral: 0")
    val labelReject = new Label("Reject: 0")
    val labelHealthy = new Label("Healthy: 0")
    val labelInfected = new Label("Infected: 0")
    val labelRecovered = new Label("Recovered: 0")

    val sliderSpeed = new Slider(1, 1000, 200) {
      showTickLabels = true
      showTickMarks = true
      majorTickUnit = 300
      minorTickCount = 2
      blockIncrement = 100
    }

    val buttonToggle = new Button("Start")
    val buttonReset = new Button("Reset")

    buttonToggle.onAction = _ => {
      isRunning = !isRunning
      buttonToggle.text = if (isRunning) "Pause" else "Resume"
    }
    buttonReset.onAction = _ => {
      isRunning = false
      population = update_mindset(populationVector(pop_size, areaSize))
      buttonToggle.text = "Start"
    }

    val layout = new VBox(10) {
      padding = Insets(10)
      children = Seq(
        canvas,
        new HBox(10, labelComply, labelNeutral, labelReject),
        new HBox(10, labelHealthy, labelInfected, labelRecovered),
        new HBox(10, new Label("Speed (ms):"), sliderSpeed, buttonToggle, buttonReset)
      )
    }

    stage = new JFXApp3.PrimaryStage {
      title.value = "Pandemia Simulation"
      scene = new Scene(canvasSize + 20, canvasSize + 100) {
        content = layout
      }
    }

    def draw(): Unit = {
      gc.fill = Color.White
      gc.fillRect(0, 0, canvasSize, canvasSize)

      for (p <- population) {
        gc.fill = p.mind_status match {
          case Comply   => Color.Green
          case Neutral  => Color.Orange
          case Reject => Color.Red
        }

        val drawX = scale(p.x, areaSize, canvasSize)
        val drawY = scale(p.y, areaSize, canvasSize)
        val size = math.max(2.0, canvasSize / areaSize.toDouble) - 1.0

        gc.fillRect(drawX, drawY, size, size)

        gc.fill = p.health_status match {
          case Healthy   => Color.Green
          case Infected  => Color.Red
          case Recovered => Color.Blue
        }
        gc.fillOval(drawX, drawY, size, size)
      }
    }

    def updateCounters(): Unit = {
      val comply = population.count(_.mind_status == Comply)
      val neutral = population.count(_.mind_status == Neutral)
      val reject = population.count(_.mind_status == Reject)
      val healthy = population.count(_.health_status == Healthy)
      val infected = population.count(_.health_status == Infected)
      val recovered = population.count(_.health_status == Recovered)

      labelComply.text = s"Comply: $comply"
      labelNeutral.text = s"Neutral: $neutral"
      labelReject.text = s"Reject: $reject"
      labelHealthy.text = s"Healthy: $healthy"
      labelInfected.text = s"Infected: $infected"
      labelRecovered.text = s"Recovered: $recovered"
      if (infected == 0) isRunning = false
    }

    val timer = AnimationTimer { now =>
      val delay = (sliderSpeed.value() * 1_000_000).toLong
      if (isRunning && now - lastUpdateTime > delay) {
        population = move(population,areaSize)
        population = update_mindset(population)
        population = infect(population, infection_radius, virus_infection_chance)
        population = observation(population, radius = observation_radius)
        draw()
        updateCounters()
        lastUpdateTime = now
      }
    }

    timer.start()
  }
}
