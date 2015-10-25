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
        val testDS = DataSet.fromFunc(3, 50, 5.0){ x => x.map(y => y*y*y).sum }

        // load DS from file
        // clean up algebra and tree based solution creation?
        // make terminal/nonterminal index to full index conversion in ExpNode?
        // write a tree simplify function

        val problem  = RegressionTree(testDS, 3)

        val diag = new Diagnostic[problem.SolutionType] {
            def log(pop: Seq[problem.SolutionType]) {
                for(ind <- pop) println(ind.fitness)
                println()
            }
        }

        //val solver   = new HillClimb(50000)
        //val solver   = new Annealing(1000, 2000)
        val solver   = new GA(popSize = 10, genMax = 3, tournamentSize=3)
        val (_,sec) = time{
            for(i <- 1 to 1){

                import scala.collection.mutable.ListBuffer
                val averages = new ListBuffer[Double]()

                val ans = solver(problem)(Diagnostic.best(averages))
                println("\t"+ans+"\n")
                for(avg <- averages) print(avg + "\t");
            }
        }
        println("Run in "+sec)

    }
}

