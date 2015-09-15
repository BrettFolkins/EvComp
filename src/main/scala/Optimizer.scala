package com

trait Optimizer {
    /**
     * Given a Problem, an optimizer well return
     * an array of fitnesses as they progressed in the optimization, and the
     * final best solution
     */
    def apply(p: Problem): (Seq[Double], p.SolutionType)
}
