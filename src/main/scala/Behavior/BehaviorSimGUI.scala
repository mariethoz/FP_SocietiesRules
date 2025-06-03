package Behavior

import Behavior.AdoptedBehavior.{mindset, move, observation}
import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp3
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.{Button, Label, Slider}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.paint.Color

import scala.util.Random


object BehaviorSimGUI extends JFXApp3 {


  val areaSize: Int = 100
  val pop_size: Int = 500
  val radius: Int = 25* areaSize/pop_size
  val canvasSize: Int = 400

  var population: Vector[Person] = Vector.tabulate(pop_size) { i =>
    Person(i, Neutral, Random.nextInt(areaSize), Random.nextInt(areaSize), Random.nextInt(100))
  }

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

    val sliderSpeed = new Slider(100, 1000, 200) {
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
      population = Vector.tabulate(pop_size) { i =>
        Person(i, Neutral, Random.nextInt(areaSize), Random.nextInt(areaSize), Random.nextInt(100))
      }
      buttonToggle.text = "Start"
    }

    val layout = new VBox(10) {
      padding = Insets(10)
      children = Seq(
        canvas,
        new HBox(10, labelComply, labelNeutral, labelReject),
        new HBox(10, new Label("Speed (ms):"), sliderSpeed, buttonToggle, buttonReset)
      )
    }

    stage = new JFXApp3.PrimaryStage {
      title.value = "Virus Simulation"
      scene = new Scene(canvasSize + 20, canvasSize + 100) {
        content = layout
      }
    }

    def draw(): Unit = {
      gc.fill = Color.White
      gc.fillRect(0, 0, canvasSize, canvasSize)

      for (p <- population) {
        gc.fill = p.status match {
          case Comply   => Color.Green
          case Neutral  => Color.Orange
          case Reject => Color.Red
        }

        val drawX = scale(p.x, areaSize, canvasSize)
        val drawY = scale(p.y, areaSize, canvasSize)
        val size = math.max(2.0, canvasSize / areaSize.toDouble) - 1.0

        gc.fillOval(drawX, drawY, size, size)
      }
    }

    def updateCounters(): Unit = {
      val healthy = population.count(_.status == Comply)
      val infected = population.count(_.status == Neutral)
      val recovered = population.count(_.status == Reject)

      labelComply.text = s"Comply: $healthy"
      labelNeutral.text = s"Neutral: $infected"
      labelReject.text = s"Reject: $recovered"
      if (infected == 0) isRunning = false
    }

    val timer = AnimationTimer { now =>
      val delay = (sliderSpeed.value() * 1_000_000).toLong
      if (isRunning && now - lastUpdateTime > delay) {
        population = population.map(p => move(p, areaSize))
        population = mindset(population)
        population = observation(population, radius = radius)
        draw()
        updateCounters()
        lastUpdateTime = now
      }
    }

    timer.start()
  }
}
