package com

import scala.util.Random

trait RSMutate {
    def apply(dna: Seq[Float], min: Float, max: Float): Seq[Float]
}

class GaussMutate(sdv: Float) extends RSMutate{
    val rand = new Random()
    def apply(dna: Seq[Float], min: Float, max: Float): Seq[Float] = {
        def m(v: Float): Float = {
            val tmp = v + ((rand nextGaussian).toFloat * sdv)
            Math.min(max, Math.max(tmp, min)).toFloat
        }
        dna.map(m(_))
    }
}
