package com

import scala.util.Random

class Spherical extends SolnSpace {
    val dim = 30
    val rand = new Random()
    type Solution = Array[Float]
    def randomVal() = (rand.nextFloat()*10.24f) - 5.12f
    def randomSol(): Solution = Array.fill[Float](dim)(randomVal())
    def fitness(s: Solution): Float = s.map(x => x*x).reduce(_+_)
    def mutate(s: Solution): Solution = {
        def gauss() = (rand nextGaussian).toFloat*0.0512f;
        s.map(x => x+gauss())
    }
}
