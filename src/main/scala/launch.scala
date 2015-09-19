package com

import com.graph._
import swing._

object App extends SimpleSwingApplication{

    val mutator   = new gaussMutate(0.05f)
    val crosser   = new twoPointCrossover()
    val problems  = List(Schwefel)//Sphere,Schwefel,Rosenbrock,Rastrigin,Ackley,Griewangk)
    val optimizer = new GA(popSize = 500, genMax = 1000)

    val t0 = System.currentTimeMillis()
    val runs = problems map { p =>
        val problem = p(mutator, crosser)
        val (averages, best) = optimizer(problem)
        //println(problem + ": " + averages)
        new ArrayDataSource(problem+" Average", averages)
    }
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0).toDouble/1000.0 + "s")

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
