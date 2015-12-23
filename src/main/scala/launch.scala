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
cleanup launch
virtual velometer
apply CGP techniques to RegressionTrees?
*/

object App {
    //val testDS = DataSet.fromFunc(4, 50, 10.0){ x => x(0)*x(0)*x(0) - x(1)/x(2) - 3*x(3) }
    //val testDS = DataSet.fromFunc(4, 50, 10.0){ x => x.map(y => y*y).sum }
    //val testDS = DataSet.fromFunc(1, 100, 2*Math.PI){ x => Math.sin(x(0)) }
    //val testDS = DataSet.fromFile("resources/GPProjectData.csv")
    //val testDS = DataSet.fromFile("resources/propData")

    val testDS = new FitnessEvalwShow{
        val range = 100.0
        val recCount    = 3
        val inputCount  = 3 + recCount
        val outputCount = 1 + recCount
        val runningTime = 10.0
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
                } else if (t < 7.0) {
                    10.0
                } else {
                    (10.0-t)*(10.0/3.0)
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
            Chart(("Position", pos), ("Setpoint", set))
        }
    }

    def randomInRange: Double = (2.0*rand.nextDouble - 1.0)*testDS.range
    val problem = new CGP(testDS, Node.algebraOps:+new Constant(()=>randomInRange),
                            rows = 500, mutateChance = 0.06) with NoCrossover

    val solver  = new GA(popSize=5, genMax=20, tournamentSize=4, eleitism=true)
    //val solver  = new GA(popSize=5, genMax=50*2000, tournamentSize=5, eleitism=true)

    val best = new ArrayBuffer[Double]()
    val Diag = new Diagnostic[problem.SolutionType]{
        def log(pop: Seq[problem.SolutionType]) {
            val fits = pop.map(x => x.fitness)
            best    += fits.min
        }
    }

    def optimize() {
        new Thread {
            override def run() = {
                val (ans,seconds) = time{ solver(problem)(Diag) }
                val soln = ans //ans.inspect.asInstanceOf[ExpNode]

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

                val handle = s"./results/${Calendar.getInstance().getTimeInMillis()}/"
                (new File(handle)).mkdir()
                val pw = new PrintWriter(new File(handle+"data.txt"))
                pw.write(results)
                pw.close();

                val charts: Seq[(Graph,String,ViewSpec)] =
                    List((Chart(("Best",best)),     "Fitness",new RTViewSpec(80.0f, 40.0f)),
                         (show(testDS,soln.eval(_)),"Results",new RTViewSpec(20.0f, 10.0f)) )
                for((g,name,vs) <- charts){
                    g.setViewSpec(vs)
                    val img = g.render(1080,1920);
                    val out = new File(handle+name+".png");
                    ImageIO.write(img, "png", out);
                }
            }
        }.start

        //(new GraphWindow( Seq(("Best", best)) )).startup(Array())
    }

    def main(args: Array[String]) = optimize()
}


