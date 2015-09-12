package com

trait Solution[T <: Solution[T]] {
    val fitness: Double
    def mutate(): T
    def crossover(other: T): (T, T)
}
