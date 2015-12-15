package com

import com.Benchmark._
import com.CGP._
import com.expTree._
import com.graph._
import com.Entropy.rand

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import swing._
import java.util.Calendar
import java.io._
import javax.imageio._
import java.awt.image.BufferedImage

object Expiriment {
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

    /*
    popSize=201, genMax=2000, tournamentSize=4
    popSize=201, genMax=2000, tournamentSize=4, NoCrossover
    popSize=5, genMax=50*2000, tournamentSize=5, NoCrossover
    */

    def genProblem(mrate: Double) =
        new CGP(testDS, nodeSet, rows = 512, mutateChance = mrate) with NoCrossover
        //new CGP(testDS, nodeSet, rows = 512, mutateChance = mrate)

    //val solver  = new GA(popSize=201, genMax=2000, tournamentSize=4, eleitism=true)
    val solver  = new GA(popSize=5, genMax=50*2000, tournamentSize=5, eleitism=true)

    val expiriment = "NoCrossover(4+1)"

    def main(args: Array[String]) {
        val log     = new PrintWriter(new File(expiriment+".log"))
        val results = new PrintWriter(new File(expiriment+".data"))

        val (data, t) = time{
            for(mrate <- (0.01 to 0.16 by 0.01)) yield {
                val problem = genProblem(mrate)
                val fits = (1 to 20).map(_.toDouble)//.map(x => solver(problem).fitness).sort
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
}
