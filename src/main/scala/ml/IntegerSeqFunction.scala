package com.ml

import com.util.Entropy.rand
import com.ml._

class IntegerSeq(fitFunc: FitnessEval, flipChance: Double, valueMax: Int)
  extends Problem {

    def validRand() = rand.nextInt(valueMax)

    type SolutionType = ISSolution
    def potential(): ISSolution = new ISSolution(
            Array.fill[Int](fitFunc.outputCount)(validRand())
        )

    class ISSolution(val coord: Seq[Int]) extends Solution[ISSolution] {
        def inspect = coord
        val fitness = fitFunc(eval(_))
        def mutate  = {
            val nc = {
                coord.updated(rand.nextInt(coord.size), validRand())
            }
            new ISSolution(nc)
        }
        def crossover(other: ISSolution) = {
            val (a,b) = {
                 (coord zip other.coord).map{ case (x, y) =>
                    if(rand.nextDouble < flipChance) (y, x)
                    else (x, y)
                } unzip
            }
            (new ISSolution(a), new ISSolution(b))
        }
        def eval(input: Seq[Double]): Seq[Double] = coord.map(_.toDouble)
        override def toString() = coord.mkString("[",", ","]")
    }


    override def toString() = s"Sequence of ${fitFunc.outputCount} Integer values"
}
