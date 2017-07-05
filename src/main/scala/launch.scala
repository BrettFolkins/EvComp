package com

import com.ml._
import com.ml.algorithm._
import com.ml.CGP._
import com.ml.expTree._
import com.util.Entropy.rand
import com.util.Chart
import com.util.Chart._
import com.util.Benchmark.time
import com.util.CSV
import com.graph._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import swing._
import java.util.Calendar
import java.io._
import javax.imageio._
import java.awt.image.BufferedImage
import java.time._
import java.time.temporal._

// clean up algebra and tree based solution creation?
// write a tree simplify function

/*
real time based running limits
CGP more ops
    change operator mutation
    change children mutation
    noop crossover
    transplant crossover
    vertical shift
Interpret final CGP results
virtual velometer
refactor quadcopter simulator
apply CGP techniques to RegressionTrees?

add some exponential like nodes to the algebra
try a simple loss-adjusted-interest rate calculation
try implementing a neural net adjuster
try having it only pick out the top N individuals
*/

object App {
    //val testDS = DataSet.fromFunc(4, 50, 10.0){ x => x(0)*x(0)*x(0) - x(1)/x(2) - 3*x(3) }
    //val testDS = DataSet.fromFunc(1, 100, 2*Math.PI){ x => Math.sin(x(0)) }
    //val testDS = DataSet.fromFile("resources/GPProjectData.csv")
    //val testDS = DataSet.fromFile("resources/propData")
    //val testDS = DataSet.fromFunc(3, 500, 10.0){ x => x.map(y => y*y).sum }




    val testDS = DataSet.fromCsv("data.csv",
            Seq("estimated_return",//"effective_rate",

                //"estimated_loss_rate",
                "lender_yield","effective_yield",
                "borrower_rate","borrower_apr",
                "listing_monthly_payment","prosper_score","listing_category_id","stated_monthly_income",
                "income_verifiable","months_employed","prior_prosper_loans","prior_prosper_loans_principal_borrowed",
                "prior_prosper_loans_principal_outstanding","monthly_debt","current_delinquencies","delinquencies_last7_years",
                "first_recorded_credit_line","credit_lines_last7_years","inquiries_last6_months","amount_delinquent",
                "current_credit_lines","open_credit_lines","bankcard_utilization","total_open_revolving_accounts","revolving_balance",
                "real_estate_payment","revolving_available_percent","total_inquiries","total_trade_items","satisfactory_accounts",
                "is_homeowner","investment_typeid"
            )
        )


    def randomInRange: Double = (2.0*rand.nextDouble - 1.0)*testDS.range
    val problem = new CGP(
                        testDS,
                        ((Node.algebraOps) :+new Constant(()=>randomInRange) ),
                        rows = 2048,
                        mutateChance = 0.02) with NoCrossover

    //val problem = RegressionTree(testDS)

    val solver  = new GA(popSize=32, genMax=1000000000, tournamentSize=2, eleitism=true)

    val best = new ArrayBuffer[Double]()
    val dgns = new Diagnostic[problem.SolutionType]{
        var count = 0
        var lastChange = 0
        def log(pop: Seq[problem.SolutionType]) {
            time()
            val fits = pop.map(x => x.fitness)
            val newBest = fits.min
            count   += 1
            if(!best.isEmpty && newBest < best.last) lastChange = count
            best    += newBest
        }

        val endTime = Instant.now().plus(2, ChronoUnit.HOURS)
        override def finished: Boolean = {
            //false//(count - lastChange > 1000)
            Instant.now().isAfter(endTime)
        }

        override def toString: String = s"Finished after $count generations"
    }

    def optimize(): Unit = {
        new Thread {
            override def run() =
                (ChartWindow( Seq(("Best", best)) )).startup(Array())
        }.start

        val (ans,seconds) = time{ solver(problem)(dgns) }
        val soln = ans
        //val soln = ans.inspect.asInstanceOf[ExpNode]
        println(soln)


        val results =
            s"""|Solved : $testDS
                |Using  : $problem
                |Running: $solver
                |Seconds: $seconds
                |Fitness: ${ans.fitness}
                |Status : $dgns
                |Answer:
                |$ans
                |""".stripMargin
        println(results)

        new ChartWindow(Chart.show(testDS, soln.eval(_))).startup(Array())

/*        val handle = s"./results/${Calendar.getInstance().getTimeInMillis()}/"
        (new File(handle)).mkdir()
        val pw = new PrintWriter(new File(handle+"data.txt"))
        pw.write(results)
        pw.close();

        val charts: Seq[(Graph,String,ViewSpec)] =
            List((Chart(("Best",best)),     "Fitness",new RTViewSpec(100.0f, 50.0f)),
                 (show(testDS,soln.eval(_)),"Results",new RTViewSpec(10.0f, 2500.0f)) )
        for((g,name,vs) <- charts){
            g.setViewSpec(vs)
            val img = g.render(1080,1920);
            val out = new File(handle+name+".png");
            ImageIO.write(img, "png", out);
        }*/
    }

    def main(args: Array[String]) = optimize()
}


