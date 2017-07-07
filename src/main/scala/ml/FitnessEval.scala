package com.ml

abstract class FitnessEval{
    //returns scale of output values
    val range: Double
    //the size of input vectors associated with this function
    val inputCount: Int
    //the size of expected output vectors
    val outputCount: Int
    //returns fitness of given function
    import FitnessEval.Vect
    def batch(func: Seq[Vect] => Seq[Vect]) : Double
    def apply(func: Vect => Vect) : Double = {
        batch { _.map{ input => func(input) } }
    }
}

object FitnessEval{
    type Vect = Seq[Double]
}
