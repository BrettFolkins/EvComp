package com

trait Solution[T <: Solution[T]] {
    val fitness: Double
    def mutate(): T
    def crossover(other: T): (T, T)
}

trait Problem {
    type SolutionType <: Solution[SolutionType]
    def potential() : SolutionType
}
