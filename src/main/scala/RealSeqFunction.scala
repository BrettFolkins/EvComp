package com

import scala.util.Random

trait RSMutate {
    def apply(dna: Seq[Float], min: Float, max: Float): Seq[Float]
}
trait RSCrossover {
    def apply(a: Seq[Float], b: Seq[Float]): (Seq[Float], Seq[Float])
}
trait RSFitness {
    def apply(dna: Seq[Float]) : Double
}

class gaussMutate(sdv: Float) extends RSMutate{
    val rand = new Random()
    def apply(dna: Seq[Float], min: Float, max: Float): Seq[Float] = {
        def m(v: Float): Float = {
            val tmp = v + ((rand nextGaussian).toFloat * sdv)
            Math.min(max, Math.max(tmp, min)).toFloat
        }
        dna.map(m(_))
    }
}

class twoPointCrossover extends RSCrossover {
    val rand = new Random()
    def apply(a: Seq[Float], b: Seq[Float]): (Seq[Float], Seq[Float]) = {
        val len = Math.min(a.size, b.size)
        val (spA,spB) = {
            val a = rand.nextInt(len+1)
            val b = rand.nextInt(len+1)
            if(a > b) (b, a)
            else (a, b)
        }
        val diff = spB-spA
        (a.patch(spA, b.slice(spA,spB), diff),
         b.patch(spA, a.slice(spA,spB), diff) )
    }
}

class nullCrossover() extends RSCrossover{
    def apply(a: Seq[Float], b: Seq[Float]): (Seq[Float], Seq[Float]) =
        (a,b)
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
