package com.RealSeqFunction

import com.util.Entropy.rand

trait RSCrossover {
    def apply(a: Seq[Double], b: Seq[Double]): (Seq[Double], Seq[Double])
}

class NullCrossover extends RSCrossover{
    def apply(a: Seq[Double], b: Seq[Double]): (Seq[Double], Seq[Double]) =
        (a,b)
}

class TwoPointCrossover extends RSCrossover {
    def apply(a: Seq[Double], b: Seq[Double]): (Seq[Double], Seq[Double]) = {
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

class ArithmeticCrossover(range: Double) extends RSCrossover {
    def apply(a: Seq[Double], b: Seq[Double]): (Seq[Double], Seq[Double]) = {
        (a zip b).map{ case (x, y) =>
            val r = rand.nextDouble * range
            (x*(1.0f-r) + y*r, x*r + y*(1.0f-r))
        } unzip
    }
}

class UniformCrossover(flipChance: Double) extends RSCrossover {
    def apply(a: Seq[Double], b: Seq[Double]): (Seq[Double], Seq[Double]) = {
        (a zip b).map{ case (x, y) =>
            if(rand.nextDouble < flipChance) (y, x)
            else (x, y)
        } unzip
    }
}
