package com

import com.CGP._
import com.expTree._
import com.graph._
import com.util.Benchmark._
import com.util.Entropy.rand

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import swing._
import java.util.Calendar
import java.io._
import javax.imageio._
import java.awt.image.BufferedImage

object Experiment {
    //val testDS = DataSet.fromFunc(4, 50, 10.0){ x => x(0)*x(0)*x(0) - x(1)/x(2) - 3*x(3) }
    val testDS = DataSet.fromFile("GPProjectData.csv")
    def randomInRange: Double = (2.0*rand.nextDouble - 1.0)*testDS.range
    val nodeSet = Node.algebraOps:+new Constant(()=>randomInRange)

    def quartiles(d: Seq[Double]): Seq[Double] = {
        val data = d.sorted
        List(data.head,
             data(data.size/4),
             data(data.size/2),
             data((data.size*3)/4),
             data.last)
    }

    val littleGA = new GA(popSize=5, genMax=50*2000, tournamentSize=5, eleitism=true)

    val bigGA = new GA(popSize=201, genMax=2000, tournamentSize=4, eleitism=true)

    def noCrossover(mrate: Double): Problem =
        new CGP(testDS, nodeSet, rows = 512, mutateChance = mrate) with NoCrossover

    def withCrossover(mrate: Double): Problem =
        new CGP(testDS, nodeSet, rows = 512, mutateChance = mrate)

    val expiriments = List(
        ("NoCrossover_4+1", littleGA, noCrossover(_)),
        ("NoCrossover_201", bigGA,    noCrossover(_)),
        ("Crossover_201",   bigGA,    withCrossover(_)),
        ("Crossover_4-1",   littleGA, withCrossover(_))
    )

    def doExpiriment(ename: String, solver: Optimizer, genProblem: (Double)=>Problem):Unit = {
        val log     = new PrintWriter(new File(ename+".log"))
        val results = new PrintWriter(new File(ename+".data"))

        log.write(genProblem(0.01)+"\n")
        log.write(solver+"\n")

        val (data, t) = time{
            for(mrate <- (0.01 to 0.1501 by 0.01)) yield {
                val problem = genProblem(mrate)
                val fits = (1 to 20).map(_.toDouble)//.map(x => solver(problem).fitness)
                val res = fits.mkString("[",",","]")
                log.write(s"$mrate $res\n")
                (mrate, fits)
            }
        }

        for( (mrate, fits) <- data ) {
            val stats = quartiles(fits).mkString(", ")
            results.write(s"$mrate, ${stats}\n")
        }

        println(s"took $t seconds")

        log.close
        results.close
    }

    def main(args: Array[String]) {
        for((name, ga, problem) <- expiriments) doExpiriment(name, ga, problem)
    }
}
