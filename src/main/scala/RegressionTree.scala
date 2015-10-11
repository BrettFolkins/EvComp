package com

import com.expTree._

import scala.util.Random

abstract class DataSet{
    val range: Double
    val vectorLen: Int
    val data: Seq[(Seq[Double],Double)]
}

object RegressionTree {
    def apply(ds: DataSet, iHeight: Int): Problem = {

        val algebra = new Algebra {
            def randomVarIndex() = rand.nextInt(ds.vectorLen)
            def randomConstantValue(): Double = ((rand.nextDouble()-0.5)*2.0)*ds.range
            case class Variable(i: Int) extends ExpNode {
                def this() = this(0)
                val children = Nil
                def buildUsing(subs: Iterator[ExpNode]) = new Variable(randomVarIndex())
                def eval(v: Any) = {
                    v match {
                        case s: Seq[Double] => s(i)
                        case _ => throw new Exception()
                    }
                }
                override def toString = "X"+i
            }
            val elements: Seq[ExpNode] = Array(new Constant(), new Variable(),
                                               new Add(),      new Subtract(),
                                               new Multiply(), new Divide()   )
        }
        import algebra._

        class Tree(val t: ExpNode) extends Solution[Tree] {
            def inspect = t
            val fitness = {
                (for((data,target) <- ds.data) yield {
                    val ans  = t.eval(data)
                    val diff = target - ans
                    diff*diff
                }).sum
            }
            def mutate(): Tree = new Tree(algebra.mutateRandomNode(t))
            def crossover(other: Tree): (Tree, Tree) = {
                val (l,r) = algebra.crossoverSubtrees(t, other.t)
                (new Tree(l), new Tree(r))
            }
            override def toString() = t.toString + " size: " + t.size
        }

        new Problem {
            val generator = new algebra.TreeGenerator(iHeight)
            type SolutionType = Tree
            def potential() = new Tree(generator.next());
        }
    }

}
