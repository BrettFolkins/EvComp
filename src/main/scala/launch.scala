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


    //val challenge = new TelemetryFilter(50.0)
/*    val challenge = new AltitudeHold(20.0){
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
            val vel = results.map{
                case (t,s) => t.velocity*t.velocity
            }.sum
            val accel = results.map{
                case (t,s) => t.acceleration*t.acceleration
            }.sum
            Math.sqrt(dist+6*vel)
        }
    }*/
    /*    val testDS = new funcSubstitution(challenge, 4, 1, flightFunc(_))
    def randomInRange: Double = (2.0*rand.nextDouble - 1.0)*testDS.range
    val problem = new CGP(testDS, Node.algebraOps:+new Constant(()=>randomInRange),
                            rows = 192, mutateChance = 0.10) with NoCrossover
    //val problem = new RegressionTree(testDS, parsimony=0.5)
*/

class RecurrentDataSet(ds: DataSet, recCount: Int) extends FitnessEvalwShow {
    val outputCount = 2 + recCount
    val inputCount = ds.inputCount
    val range = ds.range
    //val inputData = ds.data.map(_._1)
    //val expectedData = ds.data.map(_._2)
    val inputData = {
        val altacc = ds.data.map(_._1)
        altacc.map{ altacc => altacc.updated(0, altacc(0)-1723.932) }
    }
    val expectedData = {
        val pingReadings = ds.data.map(_._2)
        val altitude = pingReadings.map(x => (x/755.043941))
        val velocity = Seq(0.0) ++ altitude.sliding(3).map{ case t =>
            //calculate the velocity using the
            //centered difference of 3 samples takes 10 milliseconds apart
            (t(2)-t(0)) / (20.0/1000.0)
        } ++ Seq(0.0)
        Seq(altitude, velocity)
    }


    println(expectedData.size)
    expectedData.foreach(x=>println(x.size))
    var pos = -1
    println( this( (x:Seq[Double])=>{
        pos = pos+1
        expectedData.map(_(pos))
    } ) )
    //println( this( (x:Seq[Double])=>Seq(0.0,0.0) ) )

    println(evaluate( (x:Seq[Double])=>Seq(1.0,1.0) ).take(5))

    def evaluate(f: Seq[Double] => Seq[Double]): Seq[Seq[Double]] = {
        inputData.scanLeft(Seq.fill[Double](outputCount)(0.0)){
            case (presult, input) => f(input++presult)
        }.drop(1)
    }
    def apply(f: Seq[Double] => Seq[Double]): Double = {
        def rms(l: Seq[Double], r: Seq[Double]): Double = {
            val meansqr = l.zip(r).map{ case(a,b) => (a-b)*(a-b) }
            Math.sqrt(meansqr.sum/meansqr.size.toDouble)
        }
        val results = evaluate(f).zip(expectedData)
        val meanerrors = results.map{case (e, f) => rms(e,f)}
        //println(meanerrors)
        meanerrors.sum
    }
    def show(f: Seq[Double] => Seq[Double]): Graph = {
        val results = evaluate(f)
        //println(results)
        val resultSets: Seq[(String,Seq[Double])] = for(i <- 0 to 2) yield {
            (i.toString, results.map(_(i)) )
        }
        //val sets = ("Expected", expectedData) +: resultSets
        val expectedLabels = Seq("Expected Altitude", "Expected Velocity")
        val sets = expectedLabels.zip(expectedData) ++ resultSets

        Chart( sets.map(DataSourcePromoter(_)) : _* )
    }
}

object App {
/*    def flightFunc(K: Seq[Double])(sensors: Seq[Double]): Seq[Double] = {
        val barometer = sensors(0)
        val accelerometer = sensors(1)
        val altitudeEst = sensors(2)
        val velocityEst = sensors(3)
        val accelerationEst = sensors(4)

        //println("K: "+K)

        val altitude = barometer*K(0) + altitudeEst*(1.0-K(0))
        val acceleration = (accelerometer+1.0)*K(1) + accelerationEst*(1.0-K(1))

        val dAlt = (barometer-altitudeEst)
        val macc = (acceleration+accelerationEst)/2.0
        val velocity = K(2)*velocityEst + K(3)*dAlt + K(4)*macc

        Seq(altitude, velocity, acceleration)
    }*/

    def flightFunc(K: Seq[Double])(sensors: Seq[Double]): Seq[Double] = {
        val barometer = sensors(0)
        val accelerometer = sensors(1)
        val altitudeEst = sensors(2)
        val velocityEst = sensors(3)
        val altHOne = sensors(4)
        val altHTwo = sensors(5)

        //println("K: "+K)

        val K0 = Math.abs(K(0))
        val K1 = Math.abs(K(1))

        val altitude = barometer*K0 + altitudeEst*(1.0-K0)
        val newvelocity = (altitude-altHTwo) / (20.0/1000.0)
        val velocity = newvelocity//*K1 + velocityEst*(1.0-K1)

        Seq(altitude, velocity, altitudeEst, altHOne)
    }

    val challenge = new RecurrentDataSet(
                        DataSet.fromFile("resources/pingFlightRecording.csv"),
                        2)
    //val challenge = new TelemetryFilter(50.0)

    val testDS = new constOptimizer(challenge, /*numConsts*/5, flightFunc(_)){
        override val range = 1.0
    }
    val problem = new RealSeq(testDS,
                        new GaussMutate(/*0.2f,*/ sdv=0.0001),
                        new UniformCrossover(0.5f));

    val solver  = new GA(popSize=10, genMax=1000, tournamentSize=2, eleitism=true)
    //val solver  = new GA(popSize=100, genMax=1000, tournamentSize=2, eleitism=true)
    //val solver  = new GA(popSize=2, genMax=5, tournamentSize=2, eleitism=true)

    val bests = new ArrayBuffer[Double]()
    val dgns = new Diagnostic[problem.SolutionType]{
        val minImprovementTime = 50
        var count = 0
        var lastChange = 0
        def log(pop: Seq[problem.SolutionType]) {
            val fits = pop.map(x => x.fitness)
            //pop.foreach(p => println(p.fitness + " " + p))
            val newBest = fits.min
            println(newBest)
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

    def main(args: Array[String]) = ()//optimize()
}


