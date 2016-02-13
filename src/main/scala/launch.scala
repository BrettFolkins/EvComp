package com

import com.ml._
import com.ml.algorithm._
import com.ml.CGP._
import com.ml.expTree._
import com.util.Entropy.rand
import com.util.Chart
import com.util.Chart._
import com.util.Benchmark.time
import com.graph._
import com.Quadcopter.Quad._
import com.RealSeqFunction._
import com.ml.FitnessWrappers._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import swing._
import java.util.Calendar
import java.io._
import javax.imageio._
import java.awt.image.BufferedImage

/*
real time based running limits
CGP more ops
    change operator mutation
    change children mutation
    noop crossover
    transplant crossover
    vertical shift
Interpret final CGP results
apply CGP techniques to RegressionTrees?
expression simplify function

CGP the PID function
override fitness to include limited acceleration
*/

object App {
    def flightFunc(C: Seq[Double])(sensors: Seq[Double]): Seq[Double] = {
        val setpoint = sensors(0)
        val barometer = sensors(1)
        val accelerometer = sensors(2)
        val altitudeEst = sensors(3)
        val velocityEst = sensors(4)
        val accelerationEst = sensors(5)
        val oldIntegral = sensors(6)

        val K = Seq(0.12779056,0.27917558,0.011846306,0.0015973191,0.010109428)

        val altitude = barometer*K(0) + altitudeEst*(1.0-K(0))
        val acceleration = (accelerometer-32.17)*K(1) + accelerationEst*(1.0-K(1))

        val dAlt = (altitude-altitudeEst)/K(2)
        val macc = (acceleration+accelerationEst)/2.0
        val velocity = (1.0-K(3))*velocityEst + K(3)*dAlt + K(4)*macc

        val error = (setpoint-altitude)
        val integral = oldIntegral + error
        val throttle = C(0)*error + C(1)*velocity + C(2)*acceleration + (C(3)/100.0)*integral

        Seq(throttle, altitude, velocity, acceleration, integral)
    }

    //val challenge = new TelemetryFilter(50.0)
    val challenge = new AltitudeHold(20.0){
        override val recCount = 4
        override val range = 100.0
        override def setpoint(time: Double): Double = {
            if(time < 5.0) time*2.0
            else 10.0
        }
        override def fitnessOf(results: Seq[(Telemetry,Double)]): Double = {
            val dist = results.map{
                case (t,s) => (t.position-s)*(t.position-s)
            }.sum
            val fuel = results.map{
                case (t,s) => t.velocity*t.velocity
            }.sum
            Math.sqrt(dist+10*fuel)
        }
    }

    val testDS = new constOptimizer(challenge, 4, flightFunc(_))

    val problem = new RealSeq(testDS,
                        new SelectiveMutate(0.2f, sdv=challenge.range/1000.0),
                        new UniformCrossover(0.25f));
    val solver  = new GA(popSize=50, genMax=100, tournamentSize=3, eleitism=true)

    val bests = new ArrayBuffer[Double]()
    val dgns = new Diagnostic[problem.SolutionType]{
        val minImprovementTime = 10
        var count = 0
        var lastChange = 0
        def log(pop: Seq[problem.SolutionType]) {
            val fits = pop.map(x => x.fitness)
            val newBest = fits.min
            count   += 1
            if(!bests.isEmpty && newBest < bests.last) lastChange = count
            bests    += newBest
        }
        override def finished: Boolean = (count - lastChange > minImprovementTime)
        override def toString: String = s"Finished after $count generations"
    }

    def optimize(): Unit = {
        new Thread {
            override def run() = {
                val graph = Chart(("Best", bests))
                (new ChartWindow(graph)).startup(Array())
            }
        }.start

        val (ans,seconds) = time{ solver(problem)(dgns) }
        val soln = ans

        val results =
            s"""|Solved : $testDS
                |Using  : $problem
                |Running: $solver
                |Seconds: $seconds
                |Fitness: ${ans.fitness}
                |status : $dgns
                |Answer:
                |$ans
                |""".stripMargin
        println(results)

        (new ChartWindow( show(testDS,soln.eval(_)) )).startup(Array())
    }

    def main(args: Array[String]) = optimize()
}


