package com

trait Solution {
    val fitness: Double
    def mutate(): Solution
    def crossover(other: Solution): (Solution, Solution)
}
