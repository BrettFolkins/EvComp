package com.ml

import com.util.Chart
import com.util.Chart._
import com.graph._

package object FitnessWrappers{
    /**
     * Wraps another fitness function. Takes the result of its input funcs on
     *     Nil and applys them to the function `driver`, then returns the
     *     fitness of the resulting (Seq[Double]) => Seq[Double] function
     */
    class constOptimizer(
      toOptimize: FitnessEvalwShow,
      numConsts: Int,
      driver: (Seq[Double]) => ((Seq[Double])=>Seq[Double]))
    extends FitnessEvalwShow {
        //returns scale of output values
        val range: Double = toOptimize.range
        //the size of input vectors associated with this function
        val inputCount: Int = 0
        //the size of expected output vectors
        val outputCount: Int = numConsts
        //returns fitness of given function
        def apply(func: Seq[Double] => Seq[Double]): Double =
            toOptimize( driver(func(Nil)) )

        def show(func: Seq[Double] => Seq[Double]): Graph =
            toOptimize.show( driver(func(Nil)) )
    }
    /**
     * Wraps another fitness function. passes the evolved function to a driver
     *     function that will use it as a subcomponent of its computations
     *     returns fitness of the resulting (Seq[Double]) => Seq[Double] func
     */
    class funcSubstitution(
      toOptimize: FitnessEvalwShow,
      val inputCount: Int,
      val outputCount: Int,
      driver: ((Seq[Double])=>Seq[Double]) => ((Seq[Double])=>Seq[Double]))
    extends FitnessEvalwShow {
        //returns scale of output values
        val range: Double = toOptimize.range
        //returns fitness of given function
        def apply(func: Seq[Double] => Seq[Double]): Double =
            toOptimize( driver(func(_)) )

        def show(func: Seq[Double] => Seq[Double]): Graph =
            toOptimize.show( driver(func(_)) )
    }
}
