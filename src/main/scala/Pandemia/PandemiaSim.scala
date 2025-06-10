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
import scalafx.scene.control.{Button, Label, Slider}



object PandemiaSim extends JFXApp3 {

  // Define constants
  val observation_radius: Float = 5
  val infection_radius: Float = 3
  val virus_infection_chance: Float = 0.9

  val areaSize: Int = 100
  val canvasSize: Int = 500
  val maxPopSize: Int = 1000

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

  var pop_size: Int = 100
  var initial_infected: Int = 100
  var population = update_mindset(populationVector(size = pop_size, areaSize = areaSize, initiallyInfected = initial_infected))
  var step: Int = 0

  // Define canvas
  val canvas = new Canvas(canvasSize, canvasSize)
  val gc: GraphicsContext = canvas.graphicsContext2D

  override def start(): Unit = {

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

    val sliderSpeed = new Slider(1, 1000, 200) {
      showTickLabels = true
      showTickMarks = true
      majorTickUnit = 300
      minorTickCount = 1
      blockIncrement = 100
    }

    val sliderInfected = new Slider(0, maxPopSize, initial_infected) {
      showTickLabels = true
      showTickMarks = true
      majorTickUnit = 100
      minorTickCount = 0
      blockIncrement = 1
    }

    val slidePopSize = new Slider(1, maxPopSize, pop_size) {
      showTickLabels = true
      showTickMarks = true
      majorTickUnit = 500
      minorTickCount = 1
      blockIncrement = 1
    }

    val buttonToggle = new Button("Start")
    val buttonReset = new Button("Reset")

    // Axes
    val xAxis1 = new NumberAxis() {
      label = "Step"
    }
    val yAxis1 = new NumberAxis {
      label = "Population"
      lowerBound = 0
      upperBound = pop_size         // Fixed upper bound
      tickUnit = pop_size / 10      // Optional: set some tick spacing
      autoRanging = false // Disable auto-ranging to keep axis fixed
    }

    val xAxis2 = new NumberAxis() {
      label = "Step"
    }
    val yAxis2 = new NumberAxis {
      label = "Population"
      lowerBound = 0
      upperBound = pop_size
      tickUnit = pop_size / 10
      autoRanging = false
    }

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
      pop_size = slidePopSize.value().toInt
      initial_infected = math.min(sliderInfected.value().toInt,pop_size)
      population = update_mindset(populationVector(pop_size, areaSize, initial_infected))
      start_comply = -1
      start_neutral = -1
      start_reject = -1
      start_healthy = -1
      start_infected = -1
      start_recovered = -1

      buttonToggle.text = "Start"
      step = 0
      updateCounters()
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
            new VBox(10) {
              children = Seq(
                canvas,
                new HBox(10, labelComply, labelNeutral, labelReject),
                new HBox(10, labelHealthy, labelInfected, labelRecovered),
                new HBox(10, new Label("Speed  (ms):"), sliderSpeed, buttonToggle),
                new HBox(10, new Label("Population :"), slidePopSize),
                new HBox(10, new Label("Infected   :"), sliderInfected, buttonReset)
              )
            },
            new VBox(10) {
              children = Seq(
                healthChart,
                mindChart
              )
            }
          )
        }
      }
    }
    // Animation loop
    val timer = AnimationTimer { now =>
      val delay = (sliderSpeed.value() * 1_000_000).toLong
      if (isRunning && now - lastUpdateTime > delay) {
        simulateStep()
        drawPopulation()
        updateCounters()
        lastUpdateTime = now
        step = step + 1
      }
    }
    timer.start()
  }


  def simulateStep(): Unit = {
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
    population = infect(population, infection_radius, virus_infection_chance)
    population = observation(population, observation_radius)
    population = update_mindset(population)
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