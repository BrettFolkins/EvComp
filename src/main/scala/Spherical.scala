package com

import scala.util.Random

class Sphere protected (genome: Array[Float], sdv: Float) extends Solution {
    lazy val fitness = genome.map(x => x*x).reduce(_+_).toDouble
    def gauss() = (Sphere.rand nextGaussian).toFloat * sdv
    def mutate(): Solution = new Sphere(genome.map(x => x+gauss()), sdv)
    def crossover(other: Solution): (Solution, Solution) = {
        //fill this in later
        (new Sphere(Array(), sdv), new Sphere(Array(),sdv))
    }
    override def toString() = genome.mkString("[",",","]")
}

object Sphere {
    private val rand = new Random()
    private def randomVal() = (rand.nextFloat()*10.24f) - 5.12f
    def randomSolution(dim: Int, sdv: Float) = {
        new Sphere(Array.fill[Float](dim)(randomVal()), sdv)
    }
}
