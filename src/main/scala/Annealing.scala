package com

import scala.util.Random

object Annealing{
    val rand = new Random()

    def bypass(d: Double, t: Double): Boolean = {
        val prob = Math.exp(-d/t)
        val rnd = rand.nextFloat()
        return rand.nextFloat() < prob;
    }

    def apply(init: ()=>Solution, trials: Int, Tmax: Float): Seq[Double] = {
        def T(i: Int) = Tmax * (trials - i).toFloat / trials.toFloat
        val scores = Array.ofDim[Double](trials)
        var sol = init()
        for(i <- 0 until trials) {
            val next = sol.mutate()
            val diff = next.fitness - sol.fitness
            if(diff < 0 || bypass(diff, T(i)))
                sol = next
            scores(i) = sol.fitness
        }
        scores
    }
}
