package com.ml

import com.util._
import com.util.Chart._
import com.graph._

import scala.util.Random
import scala.io.Source

abstract class DataSet extends FitnessEvalwShow {
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
    def show(f: Seq[Double] => Seq[Double]): Graph = {
        val comparison = for((data,target) <- data) yield {
            val calculated = f(data)(0)
            (target, calculated)
        }
        val sorted = comparison.sortBy(_._1)
        Chart(("real", sorted.map{_._1}), ("calculated", sorted.map{_._2}))
    }
}

object DataSet{
    def index(key: String, keys: Seq[String]) = {
        val idx = keys.indexOf(key)
        if(idx == -1) throw new Exception(s"Couldn't find key $key")
        idx
    }

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

    def fromCsv(data: CSV, output: String, input: Seq[String]) = {
        import scala.collection.mutable.ArrayBuffer
        val dataBuffer = new ArrayBuffer[(Seq[Double],Double)]()
        data.foreach{ row =>
            val t = row(output).toDouble
            val valueBuffer = new ArrayBuffer[Double]()
            val v = input.foreach { valueBuffer += row(_).toDouble }
            dataBuffer +=( (valueBuffer.toSeq,t) )
        }
        new DataSet{
            val data = dataBuffer.toSeq
            val range = data.map(x => x._2).max
            val inputCount = data.map(x => x._1.length).min
        }
    }
}
