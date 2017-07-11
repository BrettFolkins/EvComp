package com

import org.openjdk.jmh._
import org.openjdk.jmh.annotations._

import com.ml._
import com.ml.CGP._

@State(Scope.Benchmark)
class JmhScala {
    val CGP = new CGP(
        new FitnessEval {
            val range = 1.0
            val inputCount = 10
            val outputCount = 10
            def apply(func: Seq[Double] => Seq[Double]) =
                func((1 to 10).map{_.toDouble}).sum
        },
        Node.algebra(1.0),
        2048)

    @Benchmark def randomCgpEvalute() {
        CGP.potential().fitness
    }
}
