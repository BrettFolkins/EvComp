package com

import com.Benchmark._
import com.expTree._
import com.graph._
import scala.util.Random
import swing._
import scala.collection.mutable.ArrayBuffer

// cooler mutation
// way to graph final function y vals
// better regression tree paramaterization
// clean up algebra and tree based solution creation?
// write a tree simplify function

object App {
    def main(args: Array[String]) {
        //val testDS  = DataSet.fromFunc(1, 50, 5.0){ x => Math.exp(x(0)) }
        //val testDS  = DataSet.fromFunc(3, 50, 10.0){ x => x.map(y => y*y*y).sum }
        val testDS = DataSet.fromFile("GPProjectData.csv")

        val problem = RegressionTree(testDS,
            fullHeight = 3,
            maxHeight = 6,
            parsimony = 0.50,
            crossoverBias = 0.9,
            subtreeReplaceChance = 0.10
        )
        val solver  = new GA(popSize = 201, genMax = 1000, tournamentSize=4)
        //val solver  = new GA(popSize = 5, genMax = 10, tournamentSize=2)
        //val solver  = new HillClimb(1000)

        val best    = new ArrayBuffer[Double]()
        val average = new ArrayBuffer[Double]()
        val size    = new ArrayBuffer[Double]()
        val Diag = new Diagnostic[problem.SolutionType]{
            def log(pop: Seq[problem.SolutionType]) {
                val fits = pop.map(x => x.fitness)
                best    += fits.min
                average += fits.sorted.apply(fits.size/2)//fits.sum / pop.size.toDouble
                size    += pop.map(x => x.inspect.asInstanceOf[ExpNode].size).sum / pop.size.toDouble
            }
        }

        new Thread {
            override def run() = {
                val ans = solver(problem)(Diag)
                println(ans)
                println("Fitness: "+ans.fitness)

                val tree  = ans.inspect.asInstanceOf[ExpNode]
                val correctData = testDS.data.map(x => x._2)
                val foundData = for((data,target) <- testDS.data) yield tree.eval(data)
                val res = Seq(new ArrayDataSource("Correct", correctData.sorted),
                              new ArrayDataSource("Found"  , foundData.sorted))
                (new GraphWindow(res)).startup(Array())
            }
        }.start

        val data = Seq(new ArrayDataSource("Best", best),
                       new ArrayDataSource("Median", average),
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
