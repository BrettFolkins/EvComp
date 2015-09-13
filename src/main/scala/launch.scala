package com

import com.graph._
import swing._

object App extends SimpleSwingApplication{

/*    val runs = (for(x <- (1 to 1).par) yield {
        val optimizer = new Annealing(1000000, 200)
        val (run, best) = optimizer(()=>Schwefel.randomSolution(30, 5.0f))

        new DataSource(){
            val getName = x.toString
            def get(X: Double): Double = {
                val index :Int = (X*run.size.toDouble).toInt
                run(index).toDouble
            }
        }
    }).seq*/

    val optimizer = new GA(50, 250, 3)
    val (_, best) = optimizer(()=>Sphere.randomSolution(30, 0.06f))

    val runs = List(
        new DataSource(){
            val getName = "Average"
            def get(X: Double): Double = {
                val index :Int = (X*optimizer.averages.size.toDouble).toInt
                optimizer.averages(index).toDouble
            }
        },
        new DataSource(){
            val getName = "Best"
            def get(X: Double): Double = {
                val index :Int = (X*optimizer.bests.size.toDouble).toInt
                optimizer.bests(index).toDouble
            }
        }
    )

    println(runs.size)

    def top = new MainFrame{
        title = "Optimizer"
        val aL = new java.util.ArrayList[DataSource]()
        for(ds <- runs) aL.add(ds)
        contents = Component.wrap(new Graph(aL, true))
    }
}
