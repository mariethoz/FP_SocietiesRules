package Pandemia

import scalafx.application.{JFXApp3, Platform}
import scalafx.collections.ObservableBuffer
import scalafx.embed.swing.SwingFXUtils
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.chart.{ NumberAxis, StackedAreaChart, XYChart}
import scalafx.scene.control.Button
import scalafx.scene.image.WritableImage
import scalafx.scene.layout.VBox

import java.io.File
import javax.imageio.ImageIO

case class BehaviorData(populationSize: Int, comply: Float, neutral: Float, reject: Float)

def saveChartAsPNG(chart: StackedAreaChart[Number, Number], filename: String): Unit = {
  val image = new WritableImage(chart.width.toInt, chart.height.toInt)
  chart.snapshot(null, image)
  val bufferedImage = SwingFXUtils.fromFXImage(image, null)
  ImageIO.write(bufferedImage, "png", new File(filename))
}

object PandemiaTransition extends JFXApp3 {
  // Define constants
  val limit_steps: Int = 1000
  val number_simulation: Int = 10

  val initially_infected: Int = 0

  val observation_radius: Int = 5
  val infection_radius: Float = 2
  val virus_infection_chance: Float = 0.9

  val step_pop_size: Int = 100
  val min_pop_size: Int = 100
  val max_pop_size: Int = 1000

  val area_size: Int = 100

  // Declares the population variable
  var population: Vector[Person] = Vector.empty[Person]

  def simulation(limitSteps: Int, numSim: Int): List[BehaviorData] = {
    (min_pop_size to max_pop_size by step_pop_size).map { popSize =>
      val thirdPop = (popSize.toFloat/3).toInt
      val (totalC, totalN, totalJ) = (1 to numSim).foldLeft((0, 0, 0)) { (acc, _) =>
        // Create a fresh population for each simulation run
        var pop = updateMindset(populationVector(size = popSize, areaSize = area_size, initiallyInfected = initially_infected, numReject = thirdPop, numComply = thirdPop))
        for (_ <- 0 to limitSteps) {
          pop = move(pop, area_size)
          pop = infect(pop, infection_radius, virus_infection_chance)
          pop = observation(pop, observation_radius)
          pop = updateMindset(pop)
        }
        // Calculate the ratios
        val c = pop.count(_.mind_status == Comply)
        val n = pop.count(_.mind_status == Neutral)
        val j = pop.count(_.mind_status == Reject)
        (acc._1 + c, acc._2 + n, acc._3 + j)
      }

      // Average the ratios over numSim runs:
      val avgC = totalC.toFloat / (popSize * numSim)
      val avgN = totalN.toFloat / (popSize * numSim)
      val avgJ = totalJ.toFloat / (popSize * numSim)

      println(s"$popSize -> Comply: $avgC%.2f,\tNeutral: $avgN%.2f,\tReject: $avgJ%.2f,\tTotal: ${avgC + avgN + avgJ}%.2f")

      BehaviorData(popSize, avgC, avgN, avgJ)
    }.toList
  }


  def createSeries(nameSerie: String, dataSerie: List[BehaviorData], valueExtractor: BehaviorData => Float): XYChart.Series[Number, Number] = {
    val series = new XYChart.Series[Number, Number] {
      this.name = nameSerie
    }
    dataSerie.foreach(point => series.data().add(XYChart.Data(point.populationSize, valueExtractor(point))))
    series
  }

  override def start(): Unit = {
    val results = simulation(limitSteps = limit_steps, numSim = number_simulation)

    val xAxis = new NumberAxis("Population Size", 100, 1000, 100)
    val yAxis = new NumberAxis("Ratio", 0, 1, 0.1)
    val chart = new StackedAreaChart(xAxis, yAxis)

    val buttonToggle = new Button("Save")
    buttonToggle.onAction = _ => {
      saveChartAsPNG(chart, "behavior_simulation.png")
    }

    Platform.runLater {
      chart.data = ObservableBuffer(
        createSeries("Comply", results, _.comply).delegate,
        createSeries("Neutral", results, _.neutral).delegate,
        createSeries("Reject", results, _.reject).delegate
      )
    }


    stage = new JFXApp3.PrimaryStage {
      title = "Behavioral Transition Simulation"
      scene = new Scene() {
        content = new VBox {
          spacing = 20
          padding = Insets(10)
          children = Seq(chart,buttonToggle)
        }
      }
    }

  }
}
