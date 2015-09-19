package com

import scala.util.Random

trait RSCrossover {
    def apply(a: Seq[Float], b: Seq[Float]): (Seq[Float], Seq[Float])
}

class NullCrossover extends RSCrossover{
    def apply(a: Seq[Float], b: Seq[Float]): (Seq[Float], Seq[Float]) =
        (a,b)
}

class TwoPointCrossover extends RSCrossover {
    val rand = new Random()
    def apply(a: Seq[Float], b: Seq[Float]): (Seq[Float], Seq[Float]) = {
        val len = Math.min(a.size, b.size)
        val (spA,spB) = {
            val a = rand.nextInt(len+1)
            val b = rand.nextInt(len+1)
            if(a > b) (b, a)
            else (a, b)
        }
        val diff = spB-spA
        (a.patch(spA, b.view(spA,spB), diff),
         b.patch(spA, a.view(spA,spB), diff) )
    }
}

class ArithmeticCrossover(range: Float) extends RSCrossover {
    val rand = new Random()
    def apply(a: Seq[Float], b: Seq[Float]): (Seq[Float], Seq[Float]) = {
        (a zip b).map{ case (x, y) =>
            val r = rand.nextFloat * range
            (x*(1.0f-r) + y*r, x*r + y*(1.0f-r))
        } unzip
    }
}

class UniformCrossover(flipChance: Float) extends RSCrossover {
    val rand = new Random()
    def apply(a: Seq[Float], b: Seq[Float]): (Seq[Float], Seq[Float]) = {
        (a zip b).map{ case (x, y) =>
            if(rand.nextFloat < flipChance) (y, x)
            else (x, y)
        } unzip
    }
}
