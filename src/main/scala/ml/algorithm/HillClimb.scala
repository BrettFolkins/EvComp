package com.ml.algorithm

import com.ml._

class HillClimb(trials: Int) extends Optimizer {
    def apply(p: Problem)(implicit ds: Diagnostic[p.SolutionType]): (p.SolutionType) = {
        var sol = p.potential()
        for(i <- 0 until trials) {
            val next = sol.mutate()
            if(next.fitness < sol.fitness) sol = next
            ds log Seq(sol)
        }
        sol
    }
    override def toString = s"Hill Climb for $trials iterations"
}
