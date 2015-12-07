package com

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
