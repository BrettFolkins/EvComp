package com

import swing._

object App extends SimpleSwingApplication{
    val ans = HillClimb.calc(new Schwefel(), 800)
    //println(ans.mkString("\n"))
    val dataSource = new DataSource(){
        val getName = "HillClimb"
        def get(x: Double): Double = {
            val index :Int = (x*ans.size.toDouble).toInt
            ans(index).toDouble
        }
    }

    def top = new MainFrame{
        title = "Optimizer"
        val aL = new java.util.ArrayList[DataSource]()
        aL.add(dataSource)
        contents = Component.wrap(new Graph(aL))
    }
}
