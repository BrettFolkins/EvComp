package com

import com.Benchmark._
import com.expTree._
import com.graph._
import com.CGP._
import scala.util.Random
import swing._
import scala.collection.mutable.ArrayBuffer

// clean up algebra and tree based solution creation?
// write a tree simplify function

/*
Cleanup CGP nodes
cleanup launch
CGP optimization
CGP transplant crossover
CGP change op mutation
Interpret final CGP results
virtual velometer
CGP change children mutation
CGP vertical shift operator
apply CGP techniques to RegressionTrees?
*/

object App {
    def main(args: Array[String]) {
        //val testDS  = DataSet.fromFunc(1, 50, 5.0){ x => Math.exp(x(0)) }
        val testDS  = DataSet.fromFunc(2, 50, 10.0){ x => x.map(y => y*y*y).sum }
        //val testDS = DataSet.fromFile("GPProjectData.csv")
        //val testDS = DataSet.fromFile("propData")
/*        val testDS = new FitnessEval{
            val range = 100.0
            val inputCount = 2
            def calc(func: Seq[Double] => Double) : Seq[Double] = {
                def control(q: Quad): Double =
                    func(List(q.accelerometer, q.barometer))
                    //func(List(q.position, q.velocity, q.acceleration))
                Quad.simulate(new Quad(), 10.0, control)
            }
            def apply(func: Seq[Double] => Double) : Double = {
                val results = calc(func)
                results.map(x => (10-x)*(10-x)).sum
            }
            def show(func: Seq[Double] => Double): Unit = {
                val results = calc(func)
                (new GraphWindow(List(new ArrayDataSource("height",results)))).startup(Array())
            }
        }
*/
/*        val problem = RegressionTree(testDS,
            fullHeight = 3,
            maxHeight = 6,
            parsimony = 0.01,
            crossoverBias = 0.9,
            subtreeReplaceChance = 0.10
        )*/
        val rand = new Random()
        val problem = new CGP(testDS, NodeEval.ops:+new Constant(()=>rand.nextDouble()*10.0),100)

        //val solver  = new GA(popSize = 100, genMax = 200, tournamentSize=4, eleitism=true)
        val solver = new Annealing(100*200, 200.0f)

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

        new Thread {
            override def run() = {
                val ans = solver(problem)(Diag)
                println(ans)
                println("Fitness: "+ans.fitness)

/*                val tree  = ans.inspect.asInstanceOf[ExpNode]
                testDS.show(tree.eval(_))*/

                //val func = ans.inspect.asInstanceOf[ExpNode]
                val correctData = testDS.data.map(x => x._2)
                val foundData = for((data,target) <- testDS.data) yield ans.eval(data)(0)
                val res = Seq(new ArrayDataSource("Correct", correctData.sorted),
                              new ArrayDataSource("Found"  , foundData.sorted))
                (new GraphWindow(res)).startup(Array())
            }
        }.start

        val data = Seq(new ArrayDataSource("Best", best),
                       //new ArrayDataSource("Median", average),
                       new ArrayDataSource("Size", size))
        (new GraphWindow(data)).startup(Array())
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
