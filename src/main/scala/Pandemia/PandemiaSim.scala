package Pandemia

import scalafx.application.JFXApp3
import scalafx.application.Platform
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.paint.Color
import scalafx.scene.chart.{NumberAxis, StackedAreaChart, XYChart}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.animation.AnimationTimer
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.scene.control.{Button, Label, Slider, TextField, ToggleButton}



object PandemiaSim extends JFXApp3 {

  // Define constants
  val observation_radius: Float = 5
  val infection_radius: Float = 3
  val virus_infection_chance: Float = 0.9

  val areaSize: Int = 100
  val canvasSize: Int = 500

  // Define series
  val healthySeries = new XYChart.Series[Number, Number] { name = "Healthy" }
  val infectedSeries = new XYChart.Series[Number, Number] { name = "Infected" }
  val recoveredSeries = new XYChart.Series[Number, Number] { name = "Recovered" }

  val complySeries = new XYChart.Series[Number, Number] { name = "Comply" }
  val neutralSeries = new XYChart.Series[Number, Number] { name = "Neutral" }
  val rejectSeries = new XYChart.Series[Number, Number] { name = "Reject" }

  // Simulation variable
  var isRunning = false
  var lastUpdateTime: Long = 0L
  var start_comply: Int = -1
  var start_neutral: Int = -1
  var start_reject: Int = -1
  var start_healthy: Int = -1
  var start_infected: Int = -1
  var start_recovered: Int = -1

  var pop_size: Int = 150
  var initial_infected: Int = 1
  var initial_comply: Int = 50
  var initial_reject: Int = 50

  var population = updateMindset(populationVector(pop_size, areaSize, initial_comply, initial_reject, initial_infected))
  var step: Int = 0

  // Define canvas
  val canvas = new Canvas(canvasSize, canvasSize)
  val gc: GraphicsContext = canvas.graphicsContext2D

  override def start(): Unit = {
    drawPopulation()

    val labelComply = new Label("Comply: 0/x")
    val labelNeutral = new Label("Neutral: 0/x")
    val labelReject = new Label("Reject: 0/x")
    val labelHealthy = new Label("Healthy: 0/x")
    val labelInfected = new Label("Infected: 0/x")
    val labelRecovered = new Label("Recovered: 0/x")

    def updateCounters(): Unit = {
      val comply = population.count(_.mind_status == Comply)
      val neutral = population.count(_.mind_status == Neutral)
      val reject = population.count(_.mind_status == Reject)
      val healthy = population.count(_.health_status == Healthy)
      val infected = population.count(_.health_status == Infected)
      val recovered = population.count(_.health_status == Recovered)

      if (start_reject == -1) {
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
    }


    // New text fields for simulation parameters:
    val popSizeInput = new TextField { text = "150"; prefWidth = 80 }
    val initialInfectedInput = new TextField { text = "1"; prefWidth = 80 }
    val complyInput = new TextField { text = "50"; prefWidth = 80 }
    val rejectInput = new TextField { text = "50"; prefWidth = 80 }
    val speedInput = new TextField { text = "200"; prefWidth = 80 }

    val infectionRadiusInput = new TextField { text = "3"; prefWidth = 80 }
    val observationRadiusInput = new TextField { text = "5"; prefWidth = 80 }
    val infectionChanceInput = new TextField { text = "90"; prefWidth = 80 } // percentage

    val healthInfluence = new ToggleButton("Health influence")

    val buttonToggle = new Button("Start")
    val buttonReset = new Button("Reset")

    // Axes
    val xAxis1 = new NumberAxis() { label = "Step" }
    val yAxis1 = new NumberAxis { label = "Population"; lowerBound = 0; upperBound = pop_size; tickUnit = pop_size / 10; autoRanging = false }
    val xAxis2 = new NumberAxis() { label = "Step" }
    val yAxis2 = new NumberAxis { label = "Population"; lowerBound = 0; upperBound = pop_size; tickUnit = pop_size / 10; autoRanging = false }

    buttonToggle.onAction = _ => {
      isRunning = !isRunning
      buttonToggle.text = if (isRunning) "Pause" else "Resume"
    }

    buttonReset.onAction = _ => {
      isRunning = false

      // Clear chart data to avoid sync errors
      healthySeries.data().clear()
      infectedSeries.data().clear()
      recoveredSeries.data().clear()
      complySeries.data().clear()
      neutralSeries.data().clear()
      rejectSeries.data().clear()

      // Reset state

      // Convert input values safely, with a default fallback.
      pop_size = popSizeInput.text().toIntOption.getOrElse(150)
      initial_infected = math.min(initialInfectedInput.text().toIntOption.getOrElse(1),pop_size)
      initial_comply = math.min(complyInput.text().toIntOption.getOrElse(0),pop_size)
      initial_reject = math.min(rejectInput.text().toIntOption.getOrElse(0),pop_size)

      // Create a fresh population with the specified parameters.
      population = updateMindset(populationVector(pop_size, areaSize, initial_comply, initial_reject, initial_infected))

      start_comply = -1
      start_neutral = -1
      start_reject = -1
      start_healthy = -1
      start_infected = -1
      start_recovered = -1

      buttonToggle.text = "Start"
      step = 0
      updateCounters()
      drawPopulation()
      yAxis1.upperBound = pop_size
      yAxis2.upperBound = pop_size
      yAxis1.tickUnit = pop_size/10
      yAxis2.tickUnit = pop_size/10
    }

    // Charts
    val healthChart = new StackedAreaChart(xAxis1, yAxis1)
    healthChart.title = "Health Status"
    healthChart.data = ObservableBuffer(healthySeries.delegate, infectedSeries.delegate, recoveredSeries.delegate)

    val mindChart = new StackedAreaChart(xAxis2, yAxis2)
    mindChart.title = "Mind Status"
    mindChart.data = ObservableBuffer(rejectSeries.delegate, neutralSeries.delegate, complySeries.delegate)

    stage = new JFXApp3.PrimaryStage {
      title = "Merged Simulation"
      scene = new Scene() {
        content = new HBox {
          spacing = 20
          padding = Insets(10)
          children = Seq(
            new VBox(10,
              new Label("Population Size:"), popSizeInput,
              new Label("Initial Infected:"), initialInfectedInput,
              healthInfluence,
              new Label("Initial Complying:"), complyInput,
              new Label("Initial Rejecting:"), rejectInput,
              new Label("Infection Radius:"), infectionRadiusInput,
              new Label("Observation Radius:"), observationRadiusInput,
              new Label("Infection Chance (%):"), infectionChanceInput,
              new HBox(10, labelComply, labelNeutral, labelReject),
              new HBox(10, labelHealthy, labelInfected, labelRecovered),
              new HBox(10, new Label("Speed (ms):"), speedInput, buttonToggle, buttonReset)
            ),
            new VBox(10,
                canvas
            ),
            new VBox(10,
                healthChart,
                mindChart
            )
          )
        }
      }
    }
    // Animation loop
    val timer = AnimationTimer { now =>
      val delay = (speedInput.text().toIntOption.getOrElse(200) * 1_000_000).toLong
      if (isRunning && now - lastUpdateTime > delay) {
        simulateStep(
          infectionRadiusInput.text().toFloatOption.getOrElse(3f),
          observationRadiusInput.text().toIntOption.getOrElse(5),
          infectionChanceInput.text().toFloatOption.getOrElse(90f) / 100f,
          healthInfluence.isSelected
        )
        drawPopulation()
        updateCounters()
        lastUpdateTime = now
        step = step + 1
      }
    }
    timer.start()
  }


  def simulateStep(infectionRadius: Float, observationRadius: Int, infectionChance: Float, healthInfluence: Boolean): Unit = {
    val h = population.count(_.health_status == Healthy)
    val i = population.count(_.health_status == Infected)
    val r = population.count(_.health_status == Recovered)

    val c = population.count(_.mind_status == Comply)
    val n = population.count(_.mind_status == Neutral)
    val j = population.count(_.mind_status == Reject)

    // Update UI on JavaFX thread
    Platform.runLater {
      healthySeries.data().add(XYChart.Data[Number, Number](step, h))
      infectedSeries.data().add(XYChart.Data[Number, Number](step, i))
      recoveredSeries.data().add(XYChart.Data[Number, Number](step, r))

      complySeries.data().add(XYChart.Data[Number, Number](step, c))
      neutralSeries.data().add(XYChart.Data[Number, Number](step, n))
      rejectSeries.data().add(XYChart.Data[Number, Number](step, j))
    }

    // Simulation step
    population = move(population, areaSize)
    population = infect(population, infectionRadius, infectionChance)
    population = observation(population, observationRadius, healthInfluence)
    population = updateMindset(population)
  }

  def drawPopulation(): Unit = {
    def scale(value: Int, maxLogical: Int, maxVisual: Double): Double =
      value.toDouble / maxLogical * maxVisual

    gc.fill = Color.White
    gc.fillRect(0, 0, canvasSize, canvasSize)

    for (p <- population) {
      // Fill based on health
      gc.fill = p.health_status match {
        case Healthy => Color.Green
        case Infected => Color.Red
        case Recovered => Color.LightBlue
      }

      // Outline based on mind
      gc.stroke = p.mind_status match {
        case Comply => Color.DarkGreen
        case Neutral => Color.Orange
        case Reject => Color.DarkRed
      }

      val drawX = scale(p.x, areaSize, canvasSize)
      val drawY = scale(p.y, areaSize, canvasSize)
      gc.fillOval(drawX, drawY, 6, 6)
      gc.strokeOval(drawX, drawY, 6, 6)
    }
  }
}