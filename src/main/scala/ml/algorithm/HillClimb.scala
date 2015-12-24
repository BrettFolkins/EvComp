package com.ml.algorithm

import com.ml._

import scala.util.control.Breaks._

class HillClimb(trials: Int) extends Optimizer {
    def apply(p: Problem)(implicit ds: Diagnostic[p.SolutionType]): (p.SolutionType) = {
        var sol = p.potential()
        breakable {
            for(i <- 0 until trials) {
                val next = sol.mutate()
                if(next.fitness < sol.fitness) sol = next
                ds log Seq(sol)
                if(ds.finished) break
            }
        }
        sol
    }
    override def toString = s"Hill Climb for $trials iterations"
}
