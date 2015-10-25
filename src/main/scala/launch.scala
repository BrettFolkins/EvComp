package com

import com.Benchmark._
import com.expTree._
import com.graph._
import scala.util.Random
import swing._

/*object App extends SimpleSwingApplication{

    //val mutator    = new GaussMutate(0.025f)
    //val crosser    = new ArithmeticCrossover(1.0f)
    //val problems   = List(Sphere,Schwefel,Rosenbrock,Rastrigin,Ackley,Griewangk)
    val problems   = List(Rosenbrock,Rastrigin,Schwefel)
    val optimizers = List(new GA(popSize = 500, genMax = 4000, tournamentSize=1))

    val runs = for(p <- problems; optimizer <- optimizers) yield {
        val scale   = (p.max - p.min)/200.0
        val problem = p(new SelectiveMutate(0.01f, scale.toFloat),
                        new TwoPointCrossover())
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
    class FunctionDataSource()//...
    def top = new MainFrame{
        title = "Optimizer"
        val aL = new java.util.ArrayList[DataSource]()
        for(ds <- runs) aL.add(ds)
        contents = Component.wrap(new Graph(aL, true))
    }
}
*/

object App {
    def main(args: Array[String]) {
        val testDS = new DataSet{

            //def f(X: Seq[Double]): Double = Math.sin(X(0))*100.0
            def f(X: Seq[Double]): Double = X.map(x => x*x*x).sum
            val data: Seq[(Seq[Double],Double)] = {
                val rand = new Random()
                def r()  = (rand.nextDouble - 0.5) * 10.0
                val xs   = (1 to 50).map{ i =>
                    Array.fill[Double](3)(r()).toSeq
                }
                xs.map( x => (x, f(x)) )
            }
            val range = 1.0
            val vectorLen = data.map(x => x._1.length).min

        }

        // pass Optimizer diagnostic objects that inspect populations?
        // clean up algebra and tree based solution creation?
        // make terminal/nonterminal index to full index conversion in ExpNode?
        // simplify function

        val problem  = RegressionTree(testDS, 3)
        //val solver   = new HillClimb(50000)
        //val solver   = new Annealing(1000, 2000)
        val solver   = new GA(popSize = 1500, genMax = 1000, tournamentSize=3)
        val (_,sec) = time{
            for(i <- 1 to 1){
                val (fitness,ans) = solver(problem)
                println("\t"+ans+"\n")
            }
        }
        println("Run in "+sec)

    }
}

