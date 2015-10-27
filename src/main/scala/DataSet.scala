package com

import scala.util.Random
import scala.io.Source

abstract class DataSet{
    val range: Double
    val vectorLen: Int
    val data: Seq[(Seq[Double],Double)]
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
        val vectorLen = data.map(x => x._1.length).min
    }
    def fromFile(filename: String) = {
        val source = Source.fromFile(filename).getLines().toList
        new DataSet{
            val data = for(line <- source) yield {
                val nums = line.split(',').map(x => x.toDouble).toSeq
                (nums.init, nums.last)
            }
            val range = data.map(x => x._2).max
            val vectorLen = data.map(x => x._1.length).min
        }
    }
}
