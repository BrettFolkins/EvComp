package com.RealSeqFunction

import com.util.Entropy.rand

trait RSMutate {
    def apply(dna: Seq[Double], min: Double, max: Double): Seq[Double]
}

class GaussMutate(sdv: Double) extends RSMutate{
    def apply(dna: Seq[Double], min: Double, max: Double): Seq[Double] = {
        def m(v: Double): Double = {
            val tmp = v + ((rand nextGaussian).toDouble * sdv)
            Math.min(max, Math.max(tmp, min)).toDouble
        }
        dna.map(m(_))
    }
}

class SelectiveMutate(mutateChance: Double, sdv: Double) extends RSMutate{
    def apply(dna: Seq[Double], min: Double, max: Double): Seq[Double] = {
        def m(v: Double): Double = {
            if(rand.nextDouble < mutateChance){
                val tmp = v + ((rand nextGaussian).toDouble * sdv)
                Math.min(max, Math.max(tmp, min)).toDouble
            } else {
                v
            }
        }
        dna.map(m(_))
    }
}
