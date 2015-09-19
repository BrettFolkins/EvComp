package com

import Math._

object Sphere extends RealSeqFunction("Spherical", -5.12f, 5.12f, 30, new RSFitness(){
    def apply(dna: Seq[Float]) : Double = {
        dna.map(x => x*x).reduce(_+_).toDouble
    }
})

object Schwefel extends RealSeqFunction("Schwefel", -512.03f, 511.97f, 30, new RSFitness(){
    def apply(dna: Seq[Float]) : Double = {
        def schwef(v: Float): Float = v*(sin(sqrt(abs(v))).toFloat)
        418.9829*dna.size + dna.map(schwef _).reduce(_+_)
    }
})

object Rosenbrock extends RealSeqFunction("Rosenbrock", -2.048f, 2.048f, 30, new RSFitness(){
    def apply(dna: Seq[Float]) : Double = {
        var sum = 0.0
        for(i <- 0 until (dna.size-1)) {
            val xi1 = dna(i+1)
            val xi  = dna(i)
            val a = (xi1 - xi*xi)
            val b = (xi - 1.0)
            sum += 100*a*a + b*b
        }
        sum
    }
})

object Rastrigin extends RealSeqFunction("Rastrigin", -5.12f, 5.12f, 30, new RSFitness(){
    def apply(dna: Seq[Float]) : Double = {
        def rast(v: Float) = v*v - 10*cos(2*PI*v)
        10*dna.size + dna.map(rast(_)).reduce(_+_)
    }
})

object Ackley extends RealSeqFunction("Ackley", -30.0f, 30.0f, 30, new RSFitness(){
    def apply(dna: Seq[Float]) : Double = {
        val a = dna.map(x => x*x).reduce(_+_).toDouble / dna.size.toDouble
        val b = dna.map(x => cos(2*PI*x)).reduce(_+_) / dna.size.toDouble
        20.0 + E - 20*exp(-0.2*sqrt(a)) - exp(b)
    }
})

object Griewangk extends RealSeqFunction("Griewangk", -600f, 600f, 30, new RSFitness(){
    def apply(dna: Seq[Float]) : Double = {
        val a = dna.map(x => x*x/4000).reduce(_+_).toDouble
        val b = dna.zipWithIndex.map {case (v, i) =>
            cos(v.toDouble/sqrt(i))
        }.reduce(_*_)
        1.0 + a - b
    }
})
