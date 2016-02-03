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
    val testDS = new TelemetryFilter(50.0)
    //val testDS = new AltitudeHold(10.0)

    def flightFunc(consts: Seq[Float])(sensors: Seq[Double]): Seq[Double] = {
        consts.map(_.toDouble)
    }

    def optimizeConsts(fitness: FitnessEval, numberOfConsts: Int)(
        method: (Seq[Float]) => ((Seq[Double])=>Seq[Double])
      ): Problem = {
        val constantWrapper = new RealSeqFunction(
          "Constant optimizer",
          -100.0f, 100.0f,
          numberOfConsts,
          new RSFitness(){
            def apply(dna: Seq[Float]) : Double = {
                fitness(method(dna)(_))
            }
        })
        constantWrapper(new GaussMutate(0.5f), new TwoPointCrossover())
    }

    val problem = optimizeConsts(testDS, 3){ flightFunc(_) }

/*
    def randomInRange: Double = (2.0*rand.nextDouble - 1.0)*testDS.range
    val problem = new CGP(testDS, Node.algebraOps:+new Constant(()=>randomInRange),
                            rows = 64, mutateChance = 0.10) with NoCrossover
*/

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
        val soln = ans.inspect.asInstanceOf[Seq[Float]]

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

        //(new ChartWindow( show(testDS,soln.eval(_)) )).startup(Array())
        (new ChartWindow( show(testDS,flightFunc(soln)(_))) ).startup(Array())
    }

    def main(args: Array[String]) = optimize()
}


