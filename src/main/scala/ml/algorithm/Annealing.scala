package com.ml.algorithm

import com.ml._

import scala.util.Random
import scala.util.control.Breaks._

class Annealing(
    trials: Int,
    Tmax: Float,
    TempExponent: Double = 1.0)
  extends Optimizer{
    val rand = new Random()

    /**
     * given the time as a value between 0 (start) and 1 (finish)
     * return the rate of annealing as a percentage of Tmax
     */
    def schedule(time: Float): Float = math.pow(1.0-time, TempExponent).toFloat

    def apply(p: Problem)(implicit ds: Diagnostic[p.SolutionType]): (p.SolutionType) = {
        def bypass(d: Double, i: Int): Boolean = {
            val time = i.toFloat / trials.toFloat
            val temp = Tmax * schedule(time)
            val prob = Math.exp(-d/temp)
            return rand.nextFloat() < prob; // 0 <= nextFloat <= 1
        }
        var sol = p.potential()
        breakable {
            for(i <- 0 until trials) {
                val next = sol.mutate()
                val diff = next.fitness - sol.fitness
                if(diff < 0 || bypass(diff, i))
                    sol = next
                ds log Seq(sol)
                if(ds.finished) break
            }
        }
        sol
    }

    override def toString() = "Annealing"
}
