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
import scala.io._
import swing._
import java.util.Calendar
import java.io._
import javax.imageio._
import java.awt.image.BufferedImage
import java.time._
import java.time.temporal._

object App {
    def prosperDataSet() = {
        val data = CSV.fromLines(Source.fromFile("data.csv").getLines().toSeq)
            .filter { row =>
                val e = row("effective_rate").toDouble
                e <= 0.1 && e >= -0.5
            }

        DataSet.fromCsv(data,
            "effective_rate",
            Seq("estimated_return","borrower_rate","prosper_score","bankcard_utilization"
/*                "estimated_return",
                "estimated_loss_rate",
                "lender_yield","effective_yield",
                "borrower_rate","borrower_apr",
                "listing_monthly_payment","prosper_score","listing_category_id","stated_monthly_income",
                "income_verifiable","months_employed","prior_prosper_loans","prior_prosper_loans_principal_borrowed",
                "prior_prosper_loans_principal_outstanding","monthly_debt","current_delinquencies","delinquencies_last7_years",
                "first_recorded_credit_line","credit_lines_last7_years","inquiries_last6_months","amount_delinquent",
                "current_credit_lines","open_credit_lines","bankcard_utilization","total_open_revolving_accounts","revolving_balance",
                "real_estate_payment","revolving_available_percent","total_inquiries","total_trade_items","satisfactory_accounts",
                "is_homeowner","investment_typeid",
                "amount_borrowed","principal_balance","service_fees_paid","principal_paid","interest_paid","late_fees_paid",
                "debt_sale_proceeds_received"*/
                //,"origination_date","next_payment_due_date"
            )
        )
    }

    //val testDS = prosperDataSet()
    val testDS = DataSet.fromFunc(3, 200, 1.0){ case Seq(a,b,c) =>
        a*a+b/c
    }

    val problem = new CGP(testDS,
        Node.algebra(testDS.range),
        rows = 512,
        mutateChance = 0.02) with NoCrossover

    //val problem = new RegressionTree(testDS)

    val solver = new GA(popSize=32, genMax=1000000000, tournamentSize=2, eleitism=true)

    val best = new ArrayBuffer[Double]()
    val dgns = new Diagnostic[problem.SolutionType]{
        var count = 0
        def log(pop: Seq[problem.SolutionType]) {
            val fits = pop.map(x => x.fitness)
            val newBest = fits.min
            count   += 1
            best    += newBest
        }
        val endTime = Instant.now().plus(10, ChronoUnit.SECONDS)
        override def finished: Boolean = {
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

        new ChartWindow(Chart.show(testDS, ans.eval(_))).startup(Array())

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


