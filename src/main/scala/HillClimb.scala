package com

class HillClimb(trials: Int) extends Optimizer {
    def apply[T <: Solution[T]] (gen: ()=>T): (Seq[Double], T) = {
        val scores = Array.ofDim[Double](trials)
        var sol = gen()
        for(i <- 0 until trials) {
            val next = sol.mutate()
            if(next.fitness < sol.fitness) sol = next
            scores(i) = sol.fitness
        }
        (scores, sol)
    }
}
