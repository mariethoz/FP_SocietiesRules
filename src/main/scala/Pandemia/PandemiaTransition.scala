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
  val observation_radius: Float = 5
  val infection_radius: Float = 3
  val virus_infection_chance: Float = 0.9

  val areaSize: Int = 100
  val stepPopSize: Int = 100
  val maxPopSize: Int = 1000

  var population = update_mindset(populationVector(size = 1, areaSize = areaSize, initiallyInfected = 0))

  def simulation(limitSteps: Int, numSim: Int): List[BehaviorData] = {
    (100 to maxPopSize by stepPopSize).map { popSize =>
      val (totalC, totalN, totalJ) = (1 to numSim).foldLeft((0f, 0f, 0f)) { (acc, _) =>
        // Create a fresh population for each simulation run
        var pop = update_mindset(populationVector(size = popSize, areaSize = areaSize, initiallyInfected = 0))
        for (_ <- 0 to limitSteps) {
          pop = move(pop, areaSize)
          pop = infect(pop, infection_radius, virus_infection_chance)
          pop = observation(pop, observation_radius)
          pop = update_mindset(pop)
        }
        // Calculate the ratios
        val c = pop.count(_.mind_status == Comply).toFloat / popSize
        val n = pop.count(_.mind_status == Neutral).toFloat / popSize
        val j = pop.count(_.mind_status == Reject).toFloat / popSize
        (acc._1 + c, acc._2 + n, acc._3 + j)
      }

      // Average the ratios over numSim runs:
      val avgC = totalC / numSim
      val avgN = totalN / numSim
      val avgJ = totalJ / numSim

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
    val results = simulation(30,10)

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
