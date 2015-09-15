package com

import scala.util.Random

class Sphere protected (dna: Array[Float], sdv: Float) extends Solution[Sphere]{
    lazy val fitness = dna.map(x => x*x).reduce(_+_).toDouble

    def gauss() = (Sphere.rand nextGaussian).toFloat * sdv

    def mutate(): Sphere = new Sphere(dna.map(x => x+gauss()), sdv)

    def crossover(other: Sphere): (Sphere, Sphere) = {
        //fill this in later
        (this, other)
    }

    override def toString() = dna.mkString("[",",","]")
}

object Sphere {
    private val rand = new Random()
    private def randomVal() = (rand.nextFloat()*10.24f) - 5.12f

    def apply(dimension:Int = 30, sdv:Float = 0.05f) = new Problem {
        override type SolutionType = Sphere
        def potential() : SolutionType = {
            new Sphere(Array.fill[Float](dimension)(randomVal()), sdv)
        }
    }
}
