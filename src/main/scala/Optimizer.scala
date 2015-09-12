package com

trait Optimizer {
    /**
     * Given a factory for initial solutions, an optimizer well return
     * an array of fitnesses as they progressed in the optimization, and the
     * final best solution
     */
    def apply[T <: Solution[T]] (Factory: ()=>T): (Seq[Double], T)
}
