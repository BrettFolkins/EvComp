package com

import com.graph._
import swing._

object App extends SimpleSwingApplication{

    //number of trials

    val runs = (for(x <- (1 to 10).par) yield {
        //val run = HillClimb(()=>Sphere.randomSolution(30, 0.06f), 1000)
        val (run, best) = Annealing( ()=>Sphere.randomSolution(30, 0.06f), 1000, 2);

        println("Fitness: " + best.fitness)
        println("\t"+best.toString)

        new DataSource(){
            val getName = x.toString
            def get(X: Double): Double = {
                val index :Int = (X*run.size.toDouble).toInt
                run(index).toDouble
            }
        }
    }).seq

    def top = new MainFrame{
        title = "Optimizer"
        val aL = new java.util.ArrayList[DataSource]()
        for(ds <- runs) aL.add(ds)
        contents = Component.wrap(new Graph(aL, true))
    }
}
