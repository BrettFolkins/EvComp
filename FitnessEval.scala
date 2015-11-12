package com

abstract class FitnessEval{
    //returns scale of output values
    val range: Double
    //returns the size of input vectors associated with this function
    val inputCount: Int
    //returns fitness of given function
    def apply(func: Seq[Double] => Double) : Double
}
