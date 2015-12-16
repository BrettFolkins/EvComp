package com

import com.Benchmark._
import com.CGP._
import com.expTree._
import com.graph._
import com.Entropy.rand

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
    def graph(dataSets: Seq[DataSource]): Graph = {
        val aL = new java.util.ArrayList[DataSource]()
        for(ds <- dataSets) aL.add(ds)
        new Graph(aL, true)
    }

    //val testDS = DataSet.fromFunc(4, 50, 10.0){ x => x(0)*x(0)*x(0) - x(1)/x(2) - 3*x(3) }
    //val testDS = DataSet.fromFunc(2, 50, 10.0){ x => x.map(y => y*y).sum }
    //val testDS = DataSet.fromFunc(1, 100, 2*Math.PI){ x => Math.sin(x(0)) }
    val testDS = DataSet.fromFile("GPProjectData.csv")
    //val testDS = DataSet.fromFile("propData")

/*    val testDS = new FitnessEval{
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
            val data = Seq(new ArrayDataSource("Position", pos),
                           new ArrayDataSource("Setpoint", set) )
            graph(data)
        }
    }*/

    def randomInRange: Double = (2.0*rand.nextDouble - 1.0)*testDS.range
    val problem = new CGP(testDS, Node.algebraOps:+new Constant(()=>randomInRange),
                            rows = 500, mutateChance = 0.06) //with NoCrossover

    val solver  = new GA(popSize=201, genMax=2000, tournamentSize=4, eleitism=true)
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
                val func = ans //ans.inspect.asInstanceOf[ExpNode]
/*                println(ans)
                println("Fitness: "+ans.fitness)
                println("Time:    "+seconds)

                //testDS.show(func.eval(_))
                val correctData = testDS.data.map(x => x._2)
                val foundData = for((data,target) <- testDS.data) yield func.eval(data)(0)
                val res = Seq(new ArrayDataSource("Correct", correctData.sorted),
                              new ArrayDataSource("Found"  , foundData.sorted))
                (new GraphWindow(res)).startup(Array())
*/
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

                val handle = "./results/"+Calendar.getInstance().getTimeInMillis()+"/"
                (new File(handle)).mkdir()
                val pw = new PrintWriter(new File(handle+"data.txt"))
                pw.write(results)
                pw.close();

                val bestDS = new ArrayDataSource("Best", best)
                val graphs: Seq[(Graph,String,ViewSpec)] =
                    List( (graph(List(bestDS)),      "Fitness",new RTViewSpec(80.0f, -500.0f))
                          //,(testDS.show(func.eval(_)),"Results",new RTViewSpec(20.0f, -500.0f))
                          )
                for((g,name,vs) <- graphs){
                    g.setViewSpec(vs)
                    val img = g.render(1080,1920);
                    val out = new File(handle+name+".png");
                    ImageIO.write(img, "png", out);
                }
            }
        }.start

        //live data
        val data = Seq(new ArrayDataSource("Best", best))
                       //new ArrayDataSource("Median", average),
                       //new ArrayDataSource("Size", size))
        (new GraphWindow(data)).startup(Array())
    }

    def main(args: Array[String]) {
        optimize()
    }
}

class GraphWindow(val dataSets: Seq[DataSource]) extends SimpleSwingApplication{
    def top = new MainFrame{
        title = "Optimizer"
        contents = Component.wrap(App.graph(dataSets))
    }
}

class ArrayDataSource(val getName: String, data: Seq[Double]) extends DataSource {
    def get(X: Double): Double = {
        if(data.isEmpty) return 0.0
        else {
            val index :Int = (X*data.size.toDouble).toInt
            data(index).toDouble
        }
    }
}
