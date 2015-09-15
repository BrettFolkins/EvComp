package com

class HillClimb(trials: Int) extends Optimizer {
    def apply(p: Problem): (Seq[Double], p.SolutionType) = {
        val scores = Array.ofDim[Double](trials)
        var sol = p.potential()
        for(i <- 0 until trials) {
            val next = sol.mutate()
            if(next.fitness < sol.fitness) sol = next
            scores(i) = sol.fitness
        }
        (scores, sol)
    }
}
