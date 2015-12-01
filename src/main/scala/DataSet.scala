package com

import scala.util.Random
import scala.io.Source

/*
abstract class FitnessEval{
    //returns scale of output values
    val range: Double
    //the size of input vectors associated with this function
    val inputCount: Int
    //the size of expected output vectors
    val outputCount: Int
    //returns fitness of given function
    def apply(func: Seq[Double] => Seq[Double]) : Double
}
*/


abstract class DataSet extends FitnessEval {
    val outputCount = 1
    val data: Seq[(Seq[Double],Double)]
    def apply(f: Seq[Double] => Seq[Double]): Double = {
        val ms = (for((data,target) <- data) yield {
                    val ans  = f(data)(0)
                    val diff = target - ans
                    diff*diff
                }).sum
        Math.sqrt(ms/(data.size.toDouble))
    }
}

object DataSet{
    def fromFunc(
      vecLen: Int,
      numVec: Int,
      vrange: Double)
      (f: Seq[Double] => Double) = new DataSet{
        val range = vrange
        val data: Seq[(Seq[Double],Double)] = {
            val rand = new Random()
            def r()  = (rand.nextDouble - 0.5) * 2.0 * range
            val xs   = (1 to numVec).map{ i =>
                Array.fill[Double](vecLen)(r()).toSeq
            }
            xs.map( x => (x, f(x)) )
        }
        val inputCount = data.map(x => x._1.length).min
    }
    def fromFile(filename: String) = {
        val source = Source.fromFile(filename).getLines().toList
        new DataSet{
            val data = for(line <- source) yield {
                val nums = line.split(',').map(x => x.toDouble).toSeq
                (nums.init, nums.last)
            }
            val range = data.map(x => x._2).max
            val inputCount = data.map(x => x._1.length).min
        }
    }
}
