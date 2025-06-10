package Pandemia

import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.chart.{LineChart, NumberAxis, StackedAreaChart, XYChart}
import scalafx.collections.ObservableBuffer
import scalafx.application.Platform

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.ArrayBuffer

object PandemiaSimPlot extends JFXApp3 {

  // Define constants
  val infection_radius: Float = 2f
  val virus_infection_chance: Float = 0.9f
  val observation_radius: Float = 3.0f
  val areaSize: Int = 100
  val pop_size: Int = 1000

  // Define series
  val healthySeries = new XYChart.Series[Number, Number] { name = "Healthy" }
  val infectedSeries = new XYChart.Series[Number, Number] { name = "Infected" }
  val recoveredSeries = new XYChart.Series[Number, Number] { name = "Recovered" }

  val complySeries = new XYChart.Series[Number, Number] { name = "Comply" }
  val neutralSeries = new XYChart.Series[Number, Number] { name = "Neutral" }
  val rejectSeries = new XYChart.Series[Number, Number] { name = "Reject" }


  override def start(): Unit = {
    // Axes
    val xAxis1 = new NumberAxis() { label = "Step" }
    val yAxis1 = new NumberAxis {
      label = "Population"
      lowerBound = 0
      upperBound = pop_size         // Fixed upper bound
      tickUnit = pop_size / 10      // Optional: set some tick spacing
      autoRanging = false           // Disable auto-ranging to keep axis fixed
    }


    val xAxis2 = new NumberAxis() { label = "Step" }
    val yAxis2 = new NumberAxis {
      label = "Population"
      lowerBound = 0
      upperBound = pop_size         // Adjust as needed for your mind status chart
      tickUnit = pop_size / 10
      autoRanging = false
    }

    // Charts
    val healthChart = new StackedAreaChart(xAxis1, yAxis1)
    healthChart.title = "Health Status"
    healthChart.data = ObservableBuffer(healthySeries.delegate, infectedSeries.delegate, recoveredSeries.delegate)

    val mindChart = new StackedAreaChart(xAxis2, yAxis2)
    mindChart.title = "Mind Status"
    mindChart.data = ObservableBuffer(rejectSeries.delegate, neutralSeries.delegate, complySeries.delegate)


    stage = new JFXApp3.PrimaryStage {
      title = "Simulation"
      scene = new Scene(1000, 800) {
        content = new scalafx.scene.layout.VBox {
          spacing = 10
          children = Seq(healthChart, mindChart)
        }
      }
    }

    runSimulation(steps = 100)
  }

  def runSimulation(steps: Int): Unit = {
    Future {
      var pop = populationVector(size = pop_size, areaSize = areaSize, initiallyInfected = 100)
      pop = update_mindset(pop)
      for (step <- 0 until steps) {
        val h = pop.count(_.health_status == Healthy)
        val i = pop.count(_.health_status == Infected)
        val r = pop.count(_.health_status == Recovered)

        val c = pop.count(_.mind_status == Comply)
        val n = pop.count(_.mind_status == Neutral)
        val j = pop.count(_.mind_status == Reject)

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
        pop = move(pop, areaSize)
        pop = infect(pop, infection_radius, virus_infection_chance)
        pop = observation(pop, observation_radius)
        pop = update_mindset(pop)

        Thread.sleep(100) // control update speed
      }
    }
  }
}
