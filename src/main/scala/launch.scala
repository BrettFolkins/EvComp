package com

import com.Benchmark._
import com.CGP._
import com.expTree._
import com.graph._
import com.Entropy.rand

import scala.collection.mutable.ArrayBuffer
import swing._

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
    val testDS = DataSet.fromFunc(4, 50, 10.0){ x => x(0)*x(0)*x(0) - x(1)/x(2) - 3*x(3) }
    //val testDS = DataSet.fromFunc(2, 50, 10.0){ x => x.map(y => y*y).sum }
    //val testDS = DataSet.fromFunc(1, 100, 2*Math.PI){ x => Math.sin(x(0)) }
    //val testDS = DataSet.fromFile("GPProjectData.csv")
    //val testDS = DataSet.fromFile("propData")
/*    val testDS = new FitnessEval{
        val range = 100.0
        val inputCount = 3
        val outputCount = 2
        def calc(func: Seq[Double] => Seq[Double]) : Seq[Double] = {
            var momento: Double = 0.0
            def control(q: Quad): Double = {
                val rtn = func(List(q.position, /*q.velocity,*/ q.acceleration, momento))
                momento = rtn(1)
                rtn(0)
            }
                //func(List(q.accelerometer, q.barometer))
            Quad.simulate(new Quad(), 10.0, control)
        }
        def apply(func: Seq[Double] => Seq[Double]) : Double = {
            val results = calc(func)
            results.map(x => (10-x)*(10-x)).sum
        }
        def show(func: Seq[Double] => Seq[Double]): Unit = {
            val results = calc(func)
            (new GraphWindow(List(new ArrayDataSource("height",results)))).startup(Array())
        }
    }*/


    /*val problem = RegressionTree(testDS,
            fullHeight = 3,
            maxHeight = 6,
            parsimony = 0.01,
            crossoverBias = 0.9,
            subtreeReplaceChance = 0.10
        )*/
    val problem = new CGP(testDS, Node.algebraOps:+new Constant(()=>rand.nextDouble()*10.0),100)
    val solver = new GA(popSize = 101, genMax = 1000, tournamentSize=4, eleitism=true)
    //val solver = new Annealing(100*200, 50.0f)

    val best    = new ArrayBuffer[Double]()
    //val average = new ArrayBuffer[Double]()
    val size    = new ArrayBuffer[Double]()
    val Diag = new Diagnostic[problem.SolutionType]{
        def log(pop: Seq[problem.SolutionType]) {
            val fits = pop.map(x => x.fitness)
            best    += fits.min
            //average += fits.sorted.apply(fits.size/2)//fits.sum / pop.size.toDouble
            //size    += pop.map(x => x.inspect.asInstanceOf[ExpNode].size).sum / pop.size.toDouble
        }
    }

    def optimize() {
        new Thread {
            override def run() = {
                val (ans,seconds) = time{ solver(problem)(Diag) }
                println(ans)
                println("Fitness: "+ans.fitness)
                println("Time:    "+seconds)

                //val tree  = ans.inspect.asInstanceOf[ExpNode]

                //testDS.show(tree.eval(_))
                //testDS.show(ans.eval(_)._1)

/*                val correctData = testDS.data.map(x => x._2)
                val foundData = for((data,target) <- testDS.data) yield ans.eval(data)._1(0)
                val res = Seq(new ArrayDataSource("Correct", correctData.sorted),
                              new ArrayDataSource("Found"  , foundData.sorted))
                (new GraphWindow(res)).startup(Array())*/
            }
        }.start

/*        val data = Seq(new ArrayDataSource("Best", best),
                       //new ArrayDataSource("Median", average),
                       new ArrayDataSource("Size", size))
        (new GraphWindow(data)).startup(Array())*/
    }

    def main(args: Array[String]) {
        optimize()
    }
}

class GraphWindow(val dataSets: Seq[DataSource]) extends SimpleSwingApplication{
    def top = new MainFrame{
        title = "Optimizer"
        val aL = new java.util.ArrayList[DataSource]()
        for(ds <- dataSets) aL.add(ds)
        contents = Component.wrap(new Graph(aL, true))
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
