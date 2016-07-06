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



    def pidSim(params: Seq[Double]): Double = {
        val setpoint = 10.0;

        val pid  = new PID(params(0), params(1), params(2))
        val Vpid = new PID(params(3), params(4), params(5))

        var error = 0.0;
        var value = 0.0;
        var velocity = 0.0;
        var acceleration = 0.0;

        (1 to 10000).foreach{x =>
            val Vtarget = pid.calculate(setpoint, value)
            acceleration = Vpid.calculate(Vtarget, velocity)
            velocity += acceleration
            value += velocity
            error += (setpoint-value)*(setpoint-value)
        }

        error
    }

    class Model {
        val params = scala.collection.mutable.Seq.fill[Double](6)(0.0)

        def tune(axis: Int): Unit = {
            // hill climb `axis`
        }

        def score(): Double = {
        }
    }




    val testDS = new FitnessEval{
        val range: Double = 5.0;
        val inputCount: Int = 0;
        val outputCount: Int = 6;
        val target = Seq(0, 1, 2, 3, 4, 5)
        def apply(func: Seq[Double] => Seq[Double]): Double = {
            val model = new Model()
            func(Seq()).foreach(x => model.tune(x.toInt))
            model.score()
        }
    }

    val problem = new IntegerSeq(testDS, 0.5, 6);

    val solver  = new GA(popSize=50, genMax=50, tournamentSize=2, eleitism=true)
    //val solver  = new GA(popSize=100, genMax=1000, tournamentSize=2, eleitism=true)
    //val solver  = new GA(popSize=2, genMax=5, tournamentSize=2, eleitism=true)

    val bests = new ArrayBuffer[Double]()
    val dgns = new Diagnostic[problem.SolutionType]{
        val minImprovementTime = 10
        var count = 0
        var lastChange = 0
        def log(pop: Seq[problem.SolutionType]) {
            val fits = pop.map(x => x.fitness)
            val genBest = fits.min
            println(genBest)
            count += 1
            if(!bests.isEmpty && genBest < bests.last) lastChange = count
            bests += genBest
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


