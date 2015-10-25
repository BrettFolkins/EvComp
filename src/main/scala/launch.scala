package com

import com.Benchmark._
import com.expTree._
import com.graph._
import scala.util.Random
import swing._
import scala.collection.mutable.ListBuffer

// load DS from file
// elietism
// cooler mutation
// way to graph final function
// better parsimony pressure
// graph size, fitness of population
// better regression tree paramaterization
// clean up algebra and tree based solution creation?
// write a tree simplify function

object App {
    def main(args: Array[String]) {
        val testDS  = DataSet.fromFunc(3, 50, 5.0){ x => x.map(y => y*y*y).sum }
        val problem = RegressionTree(testDS, 3)
        val solver  = new GA(popSize = 200, genMax = 200, tournamentSize=3)
        val runData = new ListBuffer[Double]()

        new Thread {
            override def run() = {
                val ans = solver(problem)(Diagnostic.best(runData))
                println(ans)
            }
        }.start

        val data = Seq(new ArrayDataSource("Best", runData))
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
