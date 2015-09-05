package com

trait SolnSpace {
    type Solution
    def randomSol() : Solution
    def mutate(s: Solution) : Solution
    def fitness(s: Solution) : Float
}
