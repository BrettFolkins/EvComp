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

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import swing._
import java.util.Calendar
import java.io._
import javax.imageio._
import java.awt.image.BufferedImage

object Experiment {
/*    val testDS = new FitnessEvalwShow{
        val range = 100.0
        val recCount    = 3
        val inputCount  = 3 + recCount
        val outputCount = 1 + recCount
        val runningTime = 20.0
        val numAverage  = 50

        override def toString =
            s"Quad Sim for $runningTime seconds with $recCount recurrent terms, averaging $numAverage trials"

        def clean(d: Double): Double =
            if( (d isInfinity) || (d isNaN) ) 0.0 else d

        def calc(func: Seq[Double] => Seq[Double]) : Seq[(Double,Double)] = {
            var momento: Seq[Double] = List.fill[Double](recCount)(0.0)

            def heightFunc(t: Double): Double = {
                if(t < 3.0){
                    t*(10.0/3.0)
                } else if (t < 17.0) {
                    10.0
                } else {
                    (20.0-t)*(10.0/3.0)
                }
            }

            def control(q: Quad, s: Double): Double = {
                //val rtn = func( List(s, q.position, q.velocity, q.acceleration)++momento )
                val rtn = func( List(s, q.barometer, q.accelerometer)++momento )
                momento = rtn.takeRight(recCount).map(clean(_))
                clean(rtn(0))
            }

            val record = new ListBuffer[(Double,Double)]()
            var quad = new Quad()
            while(quad.time < runningTime) {
                val setpoint = heightFunc(quad.time)
                quad.update(control(quad, setpoint))
                record.+=((quad.position, setpoint))
            }

            record.toList
        }
        def apply(func: Seq[Double] => Seq[Double]) : Double = {
            def score(): Double = {
                val results = calc(func)
                Math.sqrt(results.map{ case (pos, set) => (pos-set)*(pos-set) }.sum)
            }

            (1 to numAverage).map(x => score()).sum / numAverage.toDouble
        }
        def show(func: Seq[Double] => Seq[Double]): Graph = {
            val (pos, set) = calc(func).unzip
            val positionList = (1 to 5).map(x=> calc(func).unzip._1)
            val chartList = (pos +: positionList).map( ("Position",_) )
            Chart( (("Setpoint", set) +: chartList).map(DataSourcePromoter(_)):_* )
        }
    }*/

    val testDS = new FitnessEvalwShow{
        val range = 100.0
        val recCount    = 3
        val outputCount = 1 + recCount
        val inputCount  = 2 + outputCount //baro, accel, rec...
        val runningTime = 100.0
        val accelSDV: Double = 0.271 //feet per second^2
        val accelBais:Double = 0.003
        val baroSDV: Double  = 0.900 //feet

        val flightData = {
            import Math._
            val pA = 0.5*PI
            val pB = 0.1*PI
            def pos(t: Double) = 2500.0 + t*0.005 +0.3*sin(t*pA)       +sin(t*pB) //feet
            def vel(t: Double) =          1*0.005 +0.3*pA*cos(t*pA)    +pB*cos(t*pB) //feet per second
            def acc(t: Double) =          0*0.005 -0.3*pA*pA*sin(t*pA) -pB*pB*sin(t*pB) //ft/s^2
            (0.0 to runningTime by 0.05).map(t => List(pos(t), vel(t), acc(t)))
        }

        val trueVelocity = flightData.map(_(1))
        val trueAltitude = flightData.map(_(0))

        val sensorData = {
            def gaussSDV(sdv: Double) = rand.nextGaussian()*sdv
            def baroNoise(b: Double) = b+gaussSDV(baroSDV)
            def acclNoise(a: Double) = a+gaussSDV(accelSDV)+accelBais
            def mapSensors(s: List[Double]) = List(baroNoise(s(0)), acclNoise(s(2)))
            flightData.map(mapSensors(_))
        }

        override def toString =
            s"altitude filter for $runningTime seconds with $recCount recurrent terms"

        def clean(d: Double): Double =
            if( (d isInfinity) || (d isNaN) ) 0.0 else d

        def calc(func: Seq[Double] => Seq[Double]) : Seq[(Double,Double)] = {

            val results = sensorData.scanLeft(Seq.fill[Double](outputCount)(0.0)){
                case(prev,sensors) => func( (sensors ++ prev) ).map(clean(_))
            }.drop(1) //remove the 0's initialization from the outputs

            val altitudes = results.map(_.head)

            //velocities.zip(trueVelocity)
            altitudes.zip(trueAltitude)
        }
        def apply(func: Seq[Double] => Seq[Double]) : Double = {
            def score(): Double = {
                val results = calc(func)
/*                val tdiffs = results.map{ case(filtered, correct) =>
                    val diffs = filtered.zip(correct).map{case(a,b) => (a-b)*(a-b)}
                    diffs(0)/*alt*/+8.0*diffs(1)/*vel*/+diffs(2)/*acc*/
                }*/
                val tdiffs = results.map{case(f,t) => (f-t)*(f-t)}
                Math.sqrt(tdiffs.sum)
            }
            score()
        }
        def show(func: Seq[Double] => Seq[Double]): Graph = {
            val results = calc(func)
            val (falt, talt) = results.unzip

            //val (falt, talt) = results.map{case(f,t) => (f(0),t(0))}.unzip
            //val (fvel, tvel) = results.map{case(f,t) => (f(1),t(1))}.unzip
            //val (facc, tacc) = results.map{case(f,t) => (f(2),t(2))}.unzip
            Chart( ("True Altitude", talt), ("Filtered Altitude", falt) )
                  //("True Velocity", tvel), ("Filtered Velocity", fvel) )
                  //("True Altitude", talt), ("Filtered Altitude", falt),
                  //("True Acceleration", tacc), ("Filtered Acceleration", facc) )
        }
    }

    def randomInRange: Double = (2.0*rand.nextDouble - 1.0)*testDS.range
    val problem = new CGP(testDS, Node.algebraOps:+new Constant(()=>randomInRange),
                            rows = 192, mutateChance = 0.10) with NoCrossover

    val solver  = new GA(popSize=50, genMax=100000, tournamentSize=5, eleitism=true)

    val best = new ArrayBuffer[Double]()
    val dgns = new Diagnostic[problem.SolutionType]{
        val minImprovementTime = 15000
        var count = 0
        var lastChange = 0
        def log(pop: Seq[problem.SolutionType]) {
            val fits = pop.map(x => x.fitness)
            val newBest = fits.min
            count   += 1
            if(!best.isEmpty && newBest < best.last) lastChange = count
            best    += newBest
        }
        override def finished: Boolean = (count - lastChange > minImprovementTime)
        override def toString: String = s"Finished after $count generations"
    }


    def optimize(): Unit = {
        val (ans,seconds) = time{ solver(problem)(dgns) }
        val soln = ans //ans.inspect.asInstanceOf[ExpNode]

        val results =
            s"""|Solved : $testDS
                |Using  : $problem
                |Running: $solver
                |Seconds: $seconds
                |Fitness: ${ans.fitness}
                |Status : $dgns
                |Answer:
                |$ans
                |""".stripMargin
        println(results)

        val handle = s"./results/${Calendar.getInstance().getTimeInMillis()}/"
        (new File(handle)).mkdir()
        val pw = new PrintWriter(new File(handle+"data.txt"))
        pw.write(results)
        pw.close();

        val charts: Seq[(Graph,String,ViewSpec)] =
            List((Chart(("Best",best)),     "Fitness",new RTViewSpec(50.0f, 25.0f)),
                 (show(testDS,soln.eval(_)),"Results",new RTViewSpec(10.0f, 2500.0f)) )
        for((g,name,vs) <- charts){
            g.setViewSpec(vs)
            val img = g.render(1200,1920);
            val out = new File(handle+name+".png");
            ImageIO.write(img, "png", out);
        }
    }

    def main(args: Array[String]) = optimize()
}

