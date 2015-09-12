package com

import scala.util.Random

class Sphere protected (dna: Array[Float], sdv: Float) extends Solution[Sphere]{
    lazy val fitness = dna.map(x => x*x).reduce(_+_).toDouble

    def gauss() = (Sphere.rand nextGaussian).toFloat * sdv

    def mutate(): Sphere = new Sphere(dna.map(x => x+gauss()), sdv)

    def crossover(other: Sphere): (Sphere, Sphere) = {
        //fill this in later
        (new Sphere(Array(), sdv), new Sphere(Array(),sdv))
    }

    override def toString() = dna.mkString("[",",","]")
}

object Sphere {
    private val rand = new Random()
    private def randomVal() = (rand.nextFloat()*10.24f) - 5.12f

    def randomSolution(dim: Int, sdv: Float) = {
        new Sphere(Array.fill[Float](dim)(randomVal()), sdv)
    }
}
