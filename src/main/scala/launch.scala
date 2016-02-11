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
*/

object App {
    def flightFunc(consts: Seq[Double])(sensors: Seq[Double]): Seq[Double] = {
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

        val C = consts.map(_.toDouble)
        val error = (setpoint-altitude)
        val integral = oldIntegral + error
        val throttle = C(0)*error + C(1)*velocity + C(2)*acceleration + C(3)*integral

        //add I term
        //bring back adaptive termination condition
        //hand CGP my evolved sensor data

        Seq(throttle, altitude, velocity, acceleration, integral)
    }

    //val challenge = new TelemetryFilter(50.0)
    val challenge = new AltitudeHold(10.0){
        override val recCount = 4
    }

    val testDS = new constOptimizer(challenge, 4, flightFunc(_))

    val problem = new RealSeq(testDS, new SelectiveMutate(0.2f, sdv=0.05f), new UniformCrossover(0.25f));
    val solver  = new GA(popSize=40, genMax=1000, tournamentSize=3, eleitism=true)

    val bests = new ArrayBuffer[Double]()

    def optimize(): Unit = {
        new Thread {
            override def run() = {
                val graph = Chart(("Best", bests))
                (new ChartWindow(graph)).startup(Array())
            }
        }.start

        val (ans,seconds) = time{ solver(problem)(Diagnostic.best(bests)) }
        val soln = ans

        val results =
            s"""|Solved : $testDS
                |Using  : $problem
                |Running: $solver
                |Seconds: $seconds
                |Fitness: ${ans.fitness}
                |Answer:
                |$ans
                |""".stripMargin
        println(results)

        (new ChartWindow( show(testDS,soln.eval(_)) )).startup(Array())
    }

    def main(args: Array[String]) = optimize()
}


