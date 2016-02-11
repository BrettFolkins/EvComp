package com.RealSeqFunction

import com.ml.FitnessEval
import Math._

class Sphere extends FitnessEval{
    val range: Double = 5.12
    val inputCount: Int = 0
    val outputCount: Int = 30
    def apply(func: Seq[Double] => Seq[Double]) : Double = {
        val coord = func(Nil);
        coord.map(x => x*x).sum
    }
}

class Schwefel extends FitnessEval{
    val range: Double = 512.0
    val inputCount: Int = 0
    val outputCount: Int = 30
    def apply(func: Seq[Double] => Seq[Double]) : Double = {
        val coord = func(Nil);
        def schwef(v: Double): Double = v*(sin(sqrt(abs(v))).toFloat)
        418.9829*coord.size + coord.map(schwef _).sum
    }
}

class Rosenbrock extends FitnessEval{
    val range: Double = 2.048
    val inputCount: Int = 0
    val outputCount: Int = 30
    def apply(func: Seq[Double] => Seq[Double]) : Double = {
        val coord = func(Nil);
        var sum = 0.0
        for(i <- 0 until (coord.size-1)) {
            val xi1 = coord(i+1)
            val xi  = coord(i)
            val a = (xi1 - xi*xi)
            val b = (xi - 1.0)
            sum += 100*a*a + b*b
        }
        sum
    }
}

class Rastrigin extends FitnessEval{
    val range: Double = 5.12
    val inputCount: Int = 0
    val outputCount: Int = 30
    def apply(func: Seq[Double] => Seq[Double]) : Double = {
        val coord = func(Nil);
        def rast(v: Double) = v*v - 10*cos(2*PI*v)
        10*coord.size + coord.map(rast(_)).sum
    }
}

class Ackley extends FitnessEval{
    val range: Double = 30.0
    val inputCount: Int = 0
    val outputCount: Int = 30
    def apply(func: Seq[Double] => Seq[Double]) : Double = {
        val coord = func(Nil);
        val p = coord.size.toDouble
        val a = coord.map(x => x*x).reduce(_+_).toDouble / p
        val b = coord.map(x => cos(2*PI*x)).reduce(_+_) / p
        20.0 + E - 20.0*exp(-0.2*sqrt(a)) - exp(b)
    }
}

class Griewangk extends FitnessEval{
    val range: Double = 600.0
    val inputCount: Int = 0
    val outputCount: Int = 30
    def apply(func: Seq[Double] => Seq[Double]) : Double = {
        val coord = func(Nil);
        val a = coord map { x=>
            val y = x.toDouble
            x*x/4000.0
        } reduce(_+_)
        val b = coord.zipWithIndex map {case (v, i) =>
            cos(v.toDouble/sqrt(i+1.0))
        } reduce(_*_)
        1.0 + a - b
    }
}
