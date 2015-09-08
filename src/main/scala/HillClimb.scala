package com

object HillClimb{
    def apply(init: ()=>Solution, trials: Int): (Seq[Double], Solution) = {
        val scores = Array.ofDim[Double](trials)
        var sol = init()
        for(i <- 0 until trials) {
            val next = sol.mutate()
            if(next.fitness < sol.fitness) sol = next
            scores(i) = sol.fitness
        }
        (scores, sol)
    }
}
