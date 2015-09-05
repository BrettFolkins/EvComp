package com

import scala.util.Random

class Schwefel extends SolnSpace {
    val dim = 30
    val rand = new Random()
    type Solution = Array[Float]
    def randomVal() = (rand.nextFloat()*1020f) - 510f
    def randomSol(): Solution = Array.fill[Float](dim)(randomVal())
    def schwef(v: Float): Float = v*(Math.sin(Math.sqrt(Math.abs(v))).toFloat)
    def fitness(s: Solution): Float = {
        418.9829f*dim + s.map(schwef(_)).reduce(_+_)
    }
    def mutate(s: Solution): Solution = {
        def gauss() = (rand nextGaussian).toFloat*5.1f;
        s.map(x => x+gauss())
    }
}
