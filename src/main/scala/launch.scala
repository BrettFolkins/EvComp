package com

import com.graph._
import swing._

object App extends SimpleSwingApplication{

    val problems  = List(Sphere(), Schwefel())
    val optimizer = new GA(popSize = 50, genMax = 250)

    val runs = problems map { p =>
        val (averages, best) = optimizer(p)
        new ArrayDataSource(p+" Average", averages)
    }

    class ArrayDataSource(name: String, data:Seq[Double]) extends DataSource{
        val getName = name
        def get(X: Double): Double = {
            val index :Int = (X*data.size.toDouble).toInt
            data(index).toDouble
        }
    }
    def top = new MainFrame{
        title = "Optimizer"
        val aL = new java.util.ArrayList[DataSource]()
        for(ds <- runs) aL.add(ds)
        contents = Component.wrap(new Graph(aL, true))
    }
}
