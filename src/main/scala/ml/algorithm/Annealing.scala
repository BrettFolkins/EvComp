package com.ml.algorithm

import com.ml._

import scala.util.Random
import scala.util.control.Breaks._

class Annealing(
    trials: Int,
    Tmax: Float,
    TempExponent: Double = 1.0,
    InitialSolSearchSize: Int = 20)
  extends Optimizer{
    val rand = new Random()

    /**
     * given the time as a value between 0 (start) and 1 (finish)
     * return the rate of annealing as a percentage of Tmax
     */
    def schedule(time: Float): Float = math.pow(1.0-time, TempExponent).toFloat

    def apply(p: Problem)(implicit ds: Diagnostic[p.SolutionType]): (p.SolutionType) = {
        def pickInitial(): p.SolutionType = {
            val pop = Seq.fill[p.SolutionType](InitialSolSearchSize)(p.potential())
            val fitpop = pop.map(x => (x.fitness, x))
            fitpop.minBy(_._1)._2
        }
        def bypass(d: Double, i: Int): Boolean = {
            val time = i.toFloat / trials.toFloat
            val temp = Tmax * schedule(time)
            val prob = Math.exp(-d/temp)
            return rand.nextFloat() < prob; // 0 <= nextFloat <= 1
        }
        var sol = pickInitial()//p.potential()
        var lastScore = sol.fitness
        var bestSol = sol
        var bestScore = java.lang.Double.MAX_VALUE
        breakable {
            for(i <- 0 until trials) {
                val next = sol.mutate()
                val nextScore = next.fitness
                val diff = nextScore - lastScore
                if(diff < 0 || bypass(diff, i))
                    sol = next
                ds log Seq(sol)
                if(ds.finished) break

                lastScore = nextScore
                if(lastScore < bestScore){
                    bestScore = lastScore
                    bestSol = sol
                }
            }
        }
        bestSol
    }

    override def toString() = "Annealing"
}
