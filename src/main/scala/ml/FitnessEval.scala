package com.ml

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
