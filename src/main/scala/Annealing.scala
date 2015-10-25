package com

import scala.util.Random

class Annealing(
    trials: Int,
    Tmax: Float)
  extends Optimizer{
    val rand = new Random()

    def apply(p: Problem)(implicit ds: Diagnostic[p.SolutionType]): (p.SolutionType) = {
        def bypass(d: Double, i: Int): Boolean = {
            val temp = Tmax * (trials - i).toFloat / trials.toFloat
            val prob = Math.exp(-d/temp)
            return rand.nextFloat() < prob; // 0 <= nextFloat <= 1
        }
        var sol = p.potential()
        for(i <- 0 until trials) {
            val next = sol.mutate()
            val diff = next.fitness - sol.fitness
            if(diff < 0 || bypass(diff, i))
                sol = next
            ds log Seq(sol)
        }
        sol
    }

    override def toString() = "Annealing"
}
