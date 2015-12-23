package com.ml

trait Solution[T <: Solution[T]] {
    def fitness: Double
    def mutate(): T
    def crossover(other: T): (T, T)
    //Return the underlying data that defines this solution
    def inspect: Any
}

trait Problem {
    type SolutionType <: Solution[SolutionType]
    def potential() : SolutionType
}

trait Optimizer {
    /**
     * Given a Problem, an optimizer well return the best solution it finds
     * and give a Diagnostic the chance to inspect the population
     */
    def apply(p: Problem)(implicit ds: Diagnostic[p.SolutionType]): (p.SolutionType)
}

