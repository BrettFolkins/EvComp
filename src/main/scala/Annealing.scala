package com

object Annealing{
    def T(t: Double): Double = { //time curve
        0.0
    }
    def bypass(d:Float, t: Double): Boolean = {
        true
    }
    def calc(space: SolnSpace, maxTrials: Int): Seq[Float] = {
        val scores = Array.ofDim[Float](maxTrials)
        var sol = space.randomSol()
        var fit = space.fitness(sol)
        for(i <- 0 until maxTrials) {
            var next = space.mutate(sol)
            var nfit = space.fitness(next)
            if(nfit < fit || bypass(fit-nfit,T(i.toDouble/maxTrials)) ){
                sol = next
                fit = nfit
            }
            scores(i) = fit
        }
        scores
    }
}
