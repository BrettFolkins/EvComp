package com

import com.graph._
import swing._

object App extends SimpleSwingApplication{
    val runs = for(x <- 1 to 10) yield {
        val run = HillClimb.calc(new Spherical(), 1000)
        new DataSource(){
            val getName = x.toString
            def get(X: Double): Double = {
                val index :Int = (X*run.size.toDouble).toInt
                run(index).toDouble
            }
        }
    }

    def top = new MainFrame{
        title = "Optimizer"
        val aL = new java.util.ArrayList[DataSource]()
        for(ds <- runs) aL.add(ds)
        contents = Component.wrap(new Graph(aL, true))
    }
}
