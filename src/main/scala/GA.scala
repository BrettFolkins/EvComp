package com

import com.Entropy.rand

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer

class GA (
    popSize: Int = 100,
    genMax: Int = 100,
    tournamentSize: Int = 3,
    eleitism: Boolean = true
  ) extends Optimizer {

    def apply(p: Problem)(implicit ds: Diagnostic[p.SolutionType]): (p.SolutionType) = {
        var pop = List.fill[p.SolutionType](popSize)(p.potential())

        for(i <- 0 until genMax){
            val newPop = (1 to popSize by 2 par).map{ x =>
                val parentA = tournament(pop, tournamentSize)(MinOrd)
                val parentB = tournament(pop, tournamentSize)(MinOrd)

                val (childA, childB) = parentA.crossover(parentB)
                val (mchildA, mchildB) = (childA.mutate(), childB.mutate())

                //calculate fitness while parallel - children should memoize it
                mchildA.fitness
                mchildB.fitness

                List(mchildA, mchildB)
            }.flatten.to[List]

            pop = if(eleitism) (pop.max(MinOrd) +: newPop)
                  else newPop

            ds log pop
        }

        pop.max(MinOrd)
    }

    override def toString() =
        s"GA w/ popSize $popSize, genMax $genMax, tournamentSize $tournamentSize, eleitism $eleitism"

    /**
     * Takes a random sampling of `num` elements from `xs` with replacement
     * Returns a new collection of the same type as `xs` whenever possible
     * May be very slow if collection does not have quick random access
     */
    def randomTake[T, CC[X] <: Seq[X]](xs: CC[T], num: Int)
      (implicit bf: CanBuildFrom[CC[T], T, CC[T]]): CC[T] = {
        val nb  = bf(xs)

        val size = xs.size
        for(x <- 1 to num)
            nb += xs(rand.nextInt(size))

        nb.result()
    }

    class MinimizeOrd extends Ordering[Solution[_]] {
        def compare(a: Solution[_], b: Solution[_]) =
            b.fitness compare a.fitness
    }
    object MinOrd extends MinimizeOrd

    def tournament[T <: Solution[T]](xs: Seq[T], num: Int)
      (implicit ord: Ordering[Solution[_]]): T = {
        val bracket = randomTake(xs, num)
        bracket.max(ord)
    }
}
