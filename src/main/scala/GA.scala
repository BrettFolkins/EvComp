package com

import scala.collection.generic.CanBuildFrom
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

class GA (
    popSize: Int = 100,
    genMax: Int = 100,
    tournamentSize: Int = 3
  ) extends Optimizer {

    def apply(p: Problem): (Seq[Double], p.SolutionType) = {
        val averages = Array.ofDim[Double](genMax)

        var pop = Vector.fill[p.SolutionType](popSize)(p.potential())

        for(i <- 0 until genMax){
            pop = (1 to popSize by 2 par).map{ x =>
                val parentA = tournament(pop, tournamentSize)(MinOrd)
                val parentB = tournament(pop, tournamentSize)(MinOrd)
                val (childA, childB) = parentA.crossover(parentB)
                List(childA.mutate(), childB.mutate())
            }.flatten.to[Vector]

            averages(i) = pop.foldLeft(0.0)(_ + _.fitness) / pop.size.toDouble
        }

        (averages, pop.max(MinOrd))
    }

    override def toString() = "GA"

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
            nb += xs(GA.rand.nextInt(size))

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

object GA {
    private val rand = new Random()

}
