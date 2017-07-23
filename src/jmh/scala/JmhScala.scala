package com

import org.openjdk.jmh._
import org.openjdk.jmh.annotations._

import com.ml._
import com.ml.CGP._

@State(Scope.Thread)
class JmhScala {
    val inputs = (1 to 10).map{_.toDouble}
    val gridSize = 1024
    val testTreeDepth = 16

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

    val nativeTree = CGP.potential()

    val plusGen = Node.algebraOps(0)
    val constGen = Node.constantInRange(1.0)
    val fullPlusGrid =
        (0 until gridSize-testTreeDepth).map { _ => constGen.generate(Nil) } ++
        (gridSize-testTreeDepth until gridSize).map { i =>
            plusGen.generate(() => Nd(i-1))
        }

    val arrayGrid = new CGP.Grid(Array() ++ fullPlusGrid)
    val vectorGrid = new CGP.Grid(Vector() ++ fullPlusGrid)

    //12517.025 ± 181.752
    @Benchmark def randomCgpEvalute() = {
        val tree = CGP.potential()
        (tree.eval(inputs), tree.mutate())
    }

    @Benchmark def arrayTree() = {
        (arrayGrid.eval(inputs), arrayGrid.mutate())
    }

    @Benchmark def vectorTree() = {
        (vectorGrid.eval(inputs), vectorGrid.mutate())
    }
}

