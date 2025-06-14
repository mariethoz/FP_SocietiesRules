package Pandemia

import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp3
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.{Button, Label, Slider, TextField}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.paint.Color

object PandemiaSimGUI extends JFXApp3 {

  val areaSize: Int = 100
  val pop_size: Int = 150
  val canvasSize: Int = 800

  val observation_radius: Int = 5
  val infection_radius: Float = 3
  val virus_infection_chance: Float = 0.9

  var population: Vector[Person] = populationVector(pop_size, areaSize)

  var isRunning = false
  var lastUpdateTime: Long = 0L
  var start_comply: Int = -1
  var start_neutral: Int = -1
  var start_reject: Int = -1
  var start_healthy: Int = -1
  var start_infected: Int = -1
  var start_recovered: Int = -1

  override def start(): Unit = {
    def scale(value: Int, maxLogical: Int, maxVisual: Double): Double =
      value.toDouble / maxLogical * maxVisual


    val canvas = new Canvas(canvasSize, canvasSize)
    val gc = canvas.graphicsContext2D

    val labelComply = new Label("Comply: 0/x")
    val labelNeutral = new Label("Neutral: 0/x")
    val labelReject = new Label("Reject: 0/x")
    val labelHealthy = new Label("Healthy: 0/x")
    val labelInfected = new Label("Infected: 0/x")
    val labelRecovered = new Label("Recovered: 0/x")

    // New text fields for simulation parameters:
    val popSizeInput = new TextField {
      text = "150" // default population size
      prefWidth = 80
    }
    val initialInfectedInput = new TextField {
      text = "1" // default initial infected count
      prefWidth = 80
    }
    val complyInput = new TextField {
      text = "50" // default ratio of complying people (e.g., 20% of population)
      prefWidth = 80
    }
    val rejectInput = new TextField {
      text = "50" // default ratio of rejecting people (e.g., 20% of population)
      prefWidth = 80
    }
    val speedInput = new TextField {
      text = "200" // default speed in ms
      prefWidth = 80
    }


    // Main action buttons:
    val buttonToggle = new Button("Start")
    val buttonReset = new Button("Reset")

    buttonToggle.onAction = _ => {
      isRunning = !isRunning
      buttonToggle.text = if (isRunning) "Pause" else "Resume"
    }

    buttonReset.onAction = _ => {
      // When Reset is clicked, read input from our text fields.
      isRunning = false

      // Convert input values safely, with a default fallback.
      val newSize = popSizeInput.text().toIntOption.getOrElse(150)
      val newInitialInfected = initialInfectedInput.text().toIntOption.getOrElse(1)
      val newComply = complyInput.text().toIntOption.getOrElse(0)
      val newReject = rejectInput.text().toIntOption.getOrElse(0)

      // Create a fresh population with the specified parameters.
      population = updateMindset(populationVector(newSize, areaSize, newComply, newReject, newInitialInfected))

      // Reset counters.
      start_comply = -1
      start_neutral = -1
      start_reject = -1
      start_healthy = -1
      start_infected = -1
      start_recovered = -1

      buttonToggle.text = "Start"
    }

    val layout = new HBox(10) {
      padding = Insets(10)
      children = Seq(
        new VBox(10,
          new Label("Population Size:"), popSizeInput,
          new Label("Initial Infected:"), initialInfectedInput,
          new Label("Initial Complying:"), complyInput,
          new Label("Initial Rejecting:"), rejectInput,
          new HBox(10, labelComply, labelNeutral, labelReject),
          new HBox(10, labelHealthy, labelInfected, labelRecovered),
          new HBox(10, new Label("Speed (ms):"), speedInput, buttonToggle, buttonReset)
        ),
        canvas
      )
    }

    stage = new JFXApp3.PrimaryStage {
      title.value = "Pandemia Simulation"
      scene = new Scene() {
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
        val size = math.max(3.0, canvasSize / areaSize.toDouble) - 1.0

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

      if (start_reject == -1){
        start_comply = comply
        start_neutral = neutral
        start_reject = reject
        start_healthy = healthy
        start_infected = infected
        start_recovered = recovered
      }

      labelComply.text = s"Comply: $comply/$start_comply"
      labelNeutral.text = s"Neutral: $neutral/$start_neutral"
      labelReject.text = s"Reject: $reject/$start_reject"
      labelHealthy.text = s"Healthy: $healthy/$start_healthy"
      labelInfected.text = s"Infected: $infected/$start_infected"
      labelRecovered.text = s"Recovered: $recovered/$start_recovered"
      if (infected == 0) isRunning = false
    }

    val timer = AnimationTimer { now =>
      val delay = (speedInput.text().toIntOption.getOrElse(200) * 1_000_000).toLong
      if (isRunning && now - lastUpdateTime > delay) {
        population = move(population,areaSize)
        population = updateMindset(population)
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
