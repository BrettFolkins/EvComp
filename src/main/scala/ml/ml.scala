package com.ml

/**
  * Solutions represent individual points in a fitness landscape corresponding
  *     to solutions to a parent Problem object
  */
trait Solution[T <: Solution[T]] {
    def fitness: Double
    def mutate(): T
    def crossover(other: T): (T, T)
    //Return the underlying data that defines this solution
    def inspect: Any
}

/**
  * Problems define a search space by providing a source of valid potential
  *     Solution objects that can be compared, mutated, and crossed
  */
trait Problem {
    type SolutionType <: Solution[SolutionType]
    def potential() : SolutionType
}

/**
  * Given a Problem, an optimizer well return the best solution it can find
  *     and give a Diagnostic the chance to inspect the population of potential
  *     solutions when convenient. It has the option to stop early when the
  *     diagnostic's "finished" field is true.
  */
trait Optimizer {
    def apply(p: Problem)(implicit ds: Diagnostic[p.SolutionType]): (p.SolutionType)
}

