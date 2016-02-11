package com.RealSeqFunction

import com.util.Entropy.rand
import com.ml._

class RealSeq(fitFunc: FitnessEval,
              mutator: RSMutate,
              crosser: RSCrossover)
        extends Problem {

    type SolutionType = RSSolution
    def potential(): RSSolution = new RSSolution(
            Array.fill[Double](fitFunc.outputCount)(validRand())
        )

    class RSSolution(val coord: Seq[Double]) extends Solution[RSSolution] {
        def inspect = coord
        val fitness = fitFunc(eval(_))
        def mutate  = new RSSolution(mutator(coord, -fitFunc.range, fitFunc.range))
        def crossover(other: RSSolution) = {
            val (a,b) = crosser(coord, other.coord)
            (new RSSolution(a), new RSSolution(b))
        }
        def eval(input: Seq[Double]): Seq[Double] = coord
        override def toString() = coord.mkString("[",", ","]")
    }

    def validRand() = (rand.nextDouble() * 2.0 - 1) * fitFunc.range;

    override def toString() = s"Sequence of ${fitFunc.outputCount} real values"
}

/*class RealSeqFunction(
  name: String,
  val min: Float,
  val max: Float,
  dim: Int,
  fitFunc: RSFitness){
    def apply(mutator: RSMutate, crossFunc: RSCrossover) : Problem = {
        class RSSolution(val dna: Seq[Float]) extends Solution[RSSolution] {
            def inspect = dna
            val fitness = fitFunc(dna)
            def mutate() = new RSSolution(mutator(dna, min, max))
            def crossover(other: RSSolution) = {
                val (a,b) = crossFunc(dna, other.dna)
                (new RSSolution(a), new RSSolution(b))
            }
            override def toString() = name+" "+dna.mkString("[",",","]")
        }

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
*/
