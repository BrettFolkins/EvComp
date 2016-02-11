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

// clean up algebra and tree based solution creation?
// write a tree simplify function

/*
real time based running limits
CGP more ops
    change operator mutation
    change children mutation
    noop crossover
    transplant crossover
    vertical shift
Interpret final CGP results
virtual velometer
refactor quadcopter simulator
apply CGP techniques to RegressionTrees?
*/

object App {
    def flightFunc(consts: Seq[Double])(sensors: Seq[Double]): Seq[Double] = {
        consts
    }

    val challenge = new TelemetryFilter(50.0)
    //val challenge = new AltitudeHold(10.0)

    val testDS = new constOptimizer(challenge, 3, flightFunc(_))

    val problem = new RealSeq(testDS, new GaussMutate(0.5f), new TwoPointCrossover());
    val solver  = new GA(popSize=40, genMax=100, tournamentSize=3, eleitism=true)

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


