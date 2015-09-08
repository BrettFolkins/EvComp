package com

import scala.util.Random

class Schwefel protected (genome: Array[Float], sdv: Float) extends Solution {
    def schwef(v: Float): Float = v*(Math.sin(Math.sqrt(Math.abs(v))).toFloat)
    lazy val fitness = 418.9829*genome.size + genome.map(schwef(_)).reduce(_+_)
    def mutate(): Solution = {
        def bound(min: Float, v: Float, max: Float) = Math.min(max, Math.max(v,min))
        def gauss() = (Schwefel.rand nextGaussian).toFloat * sdv
        val rawGenome = genome.map(x => x+gauss())
        val newGenome = rawGenome.map(x => bound(-512.03f, x, 511.97f))
        new Schwefel(newGenome, sdv)
    }
    def crossover(other: Solution): (Solution, Solution) = {
        //fill this in later
        (new Schwefel(Array(), sdv), new Schwefel(Array(),sdv))
    }
    override def toString() = genome.mkString("[",",","]")
}

object Schwefel {
    private val rand = new Random()
    private def randomVal() = (rand.nextFloat()*1020f) - 510f
    def randomSolution(dim: Int, sdv: Float) = { //5.1
        new Schwefel(Array.fill[Float](dim)(randomVal()), sdv)
    }
}
