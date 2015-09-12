package com

import scala.util.Random
import Math._

class Schwefel protected (dna: Array[Float], sdv: Float) extends Solution[Schwefel] {
    lazy val fitness =
      418.9829*dna.size + dna.map(schwef(_)).reduce(_+_)

    def schwef(v: Float): Float =
      v*(sin(sqrt(abs(v))).toFloat)

    def mutate(): Schwefel = {
        def mute(v: Float): Float = {
            val tmp = v + ((Schwefel.rand nextGaussian).toFloat * sdv)
            min(511.97, max(tmp,-512.03)).toFloat
        }
        new Schwefel(dna.map(mute _), sdv)
    }

    def crossover(other: Schwefel): (Schwefel, Schwefel) = {
        //fill this in later
        (new Schwefel(Array(), sdv), new Schwefel(Array(),sdv))
    }

    override def toString() = dna.mkString("[",",","]")
}

object Schwefel {
    private val rand = new Random()
    private def randomVal() = (rand.nextFloat()*1020f) - 510f
    def randomSolution(dim: Int, sdv: Float) = { //5.1
        new Schwefel(Array.fill[Float](dim)(randomVal()), sdv)
    }
}
