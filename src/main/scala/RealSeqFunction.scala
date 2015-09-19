package com

import scala.util.Random

trait RSFitness {
    def apply(dna: Seq[Float]) : Double
}

class RealSeqFunction(name: String, min: Float, max: Float, dim: Int, fitFunc: RSFitness){
    def apply(mutator: RSMutate, crossFunc: RSCrossover) : Problem = {
        class RSSolution(val dna: Seq[Float]) extends Solution[RSSolution] {
            val fitness = fitFunc(dna)
            def mutate() = new RSSolution(mutator(dna, min, max))
            def crossover(other: RSSolution) = {
                val (a,b) = crossFunc(dna, other.dna)
                (new RSSolution(a), new RSSolution(b))
            }
            override def toString() = name+" "+dna.mkString("[",",","]")
        }

        val rand = new Random()
        def validRand(): Float = (rand.nextFloat() * (max-min)) + (min)
        def initial(): Seq[Float] = Array.fill[Float](dim)(validRand())

        new Problem {
            type SolutionType = RSSolution
            def potential() = new RSSolution(initial());
            override def toString() = name
        }
    }
    override def toString() = name
}
