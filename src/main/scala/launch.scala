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
    //val testDS = new TelemetryFilter(50.0)
    val testDS = new AltitudeHold(10.0){
        override def setpoint(t: Double) = {
            if(t < 5.0) t*2.0
            else 10.0
        }
    }

    def flightFunc(consts: Seq[Float])(sensors: Seq[Double]): Seq[Double] = {
        val setpoint = sensors(0)
        val barometer = sensors(1)
        val accelerometer = sensors(2)
        val altitudeEst = sensors(3)
        val velocityEst = sensors(4)
        val accelerationEst = sensors(5)

        val K = Seq(0.12779056,0.27917558,0.011846306,0.0015973191,0.010109428)

        val altitude = barometer*K(0) + altitudeEst*(1.0-K(0))
        val acceleration = (accelerometer-32.17)*K(1) + accelerationEst*(1.0-K(1))

        val dAlt = (altitude-altitudeEst)/K(2)
        val macc = (acceleration+accelerationEst)/2.0
        val velocity = (1.0-K(3))*velocityEst + K(3)*dAlt + K(4)*macc

        val C = consts.map(_.toDouble)
        val error = (setpoint-altitude)
        val intergral = oldIntegral + error
        val throttle = C(0)*error + C(1)*velocity + C(2)*acceleration + C(3)*integral

        //add I term
        //bring back adaptive termination condition
        //hand CGP my evolved sensor data

        Seq(throttle, altitude, velocity, acceleration)
    }

    def optimizeConsts(fitness: FitnessEval, numberOfConsts: Int)(
        method: (Seq[Float]) => ((Seq[Double])=>Seq[Double])
      ): Problem = {
        val constantWrapper = new RealSeqFunction(
          "Constant optimizer",
          -60.0f, 60.0f,
          numberOfConsts,
          new RSFitness(){
            def apply(dna: Seq[Float]) : Double = {
                fitness(method(dna)(_))
            }
        })
        constantWrapper(new SelectiveMutate(0.2f, sdv=0.05f), new UniformCrossover(0.25f))
    }

    val problem = optimizeConsts(testDS, 3){ flightFunc(_) }

/*
    def randomInRange: Double = (2.0*rand.nextDouble - 1.0)*testDS.range
    val problem = new CGP(testDS, Node.algebraOps:+new Constant(()=>randomInRange),
                            rows = 64, mutateChance = 0.10) with NoCrossover
*/

    val solver  = new GA(popSize=20, genMax=5000, tournamentSize=4, eleitism=true)

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


