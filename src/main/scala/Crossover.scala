package com

import scala.util.Random

trait RSCrossover {
    def apply(a: Seq[Float], b: Seq[Float]): (Seq[Float], Seq[Float])
}

class nullCrossover extends RSCrossover{
    def apply(a: Seq[Float], b: Seq[Float]): (Seq[Float], Seq[Float]) =
        (a,b)
}

class twoPointCrossover extends RSCrossover {
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
        (a.patch(spA, b.slice(spA,spB), diff),
         b.patch(spA, a.slice(spA,spB), diff) )
    }
}
