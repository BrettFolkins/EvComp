package com

import org.openjdk.jmh._
import org.openjdk.jmh.annotations._

import com.ml._
import com.ml.CGP._

@State(Scope.Benchmark)
class JmhScala {
    val inputs = (1 to 10).map{_.toDouble}
    val gridSize = 16

    val CGP = new CGP(
        new FitnessEval {
            val range = 1.0
            val inputCount = 10
            val outputCount = 1
            def apply(func: Seq[Double] => Seq[Double]) =
                func(inputs).sum
        },
        Node.algebra(1.0),
        gridSize)

    val plusGrid = {
        val plusGen = Node.algebraOps(0)
        val constGen = Node.constantInRange(1.0)
        new CGP.Grid(
            //510 plus nodes pointing to their own index -1, -2
            //2 constants at the front
            Array(constGen.generate(Nil)) ++
            (1 to gridSize).map{ i =>
                plusGen.generate(() => Nd(i-1))
            }
        )
    }

    val plusGridVector = {
        val plusGen = Node.algebraOps(0)
        val constGen = Node.constantInRange(1.0)
        new CGP.Grid(
            (Vector(constGen.generate(Nil)) ++
            (1 to gridSize).map{ i =>
                plusGen.generate(() => Nd(i-1))
            }).toArray.toSeq
        )
    }

    @Benchmark def randomCgpEvalute() = {
        CGP.potential().eval(inputs)
    }

    @Benchmark def fullPlusTree() = {
        plusGrid.eval(inputs)
    }

    @Benchmark def fullPlusTreeVector() = {
        plusGridVector.eval(inputs)
    }
}

