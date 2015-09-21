package com

import com.graph._
import swing._

object App extends SimpleSwingApplication{

    //val mutator    = new GaussMutate(0.025f)
    //val crosser    = new ArithmeticCrossover(1.0f)
    //val problems   = List(Sphere,Schwefel,Rosenbrock,Rastrigin,Ackley,Griewangk)
    val problems   = List(Rosenbrock,Rastrigin,Ackley,Griewangk)
    val optimizers = List(new GA(popSize = 500, genMax = 4000, tournamentSize=3))
        /*,
                          new Annealing(2000000,10),
                          new HillClimb(2000000))*/


/*val t0 = System.currentTimeMillis()
    for(p <- problems; optimizer<-optimizers) {
        val scale   = (p.max - p.min)/200.0
        val problem = p(new SelectiveMutate(0.05f, scale.toFloat),
                        new TwoPointCrossover())

        val results = for(x <- 1 to 10) yield {
            val (_, best) = optimizer(problem)
            best.fitness
        }

        val average = results.sum / results.size.toDouble
        val best = results.min
        println(optimizer+" on "+problem+": ")
        println("\tavg: "+average)
        println("\tmin: "+best)
    }
val t1 = System.currentTimeMillis()
println("Elapsed time: " + (t1 - t0).toDouble/1000.0 + "s")
*/

    val runs = for(p <- problems; optimizer <- optimizers) yield {
        val scale   = (p.max - p.min)/200.0
        val problem = p(new SelectiveMutate(0.05f, scale.toFloat),
                        new ArithmeticCrossover(1.0f))
        val (averages, best) = optimizer(problem)
        new ArrayDataSource(optimizer+": "+problem+" Average", averages)
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
