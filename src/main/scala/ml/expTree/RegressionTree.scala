package com.ml.expTree

import com.util.Entropy.rand
import com.ml._

class RegressionTree(
    val fit: FitnessEval,
    val fullHeight:Int = 3,
    val maxHeight: Int = 6,
    val parsimony: Double = 1.0,
    val crossoverBias: Double = 0.9,
    val subtreeReplaceChance: Double = 0.02) extends Problem {

    assert(fit.outputCount == 1, "Regression tree's only support one output")

    type SolutionType = Tree
    def potential() = new Tree(new Sapling(maxHeight, fullHeight).next())

    val nodeSet = new Algebra {
        def randomConstantValue(): Double = ((rand.nextDouble()-0.5)*2.0)*fit.range
        def randomVarIndex() = rand.nextInt(fit.inputCount)
        case class Variable(i: Int) extends ExpNode {
            def this() = this(randomVarIndex())
            val children = Nil
            def buildUsing(subs: Iterator[ExpNode]) = new Variable(randomVarIndex())
            def eval(v: Any) = {
                v match {
                    case s: Seq[Double @unchecked] => s(i)
                    case _ => throw new Exception()
                }
            }
            override def toString = "X"+i
        }
        val elements: Seq[ExpNode] = Array(new Constant(), new Variable(),
                                           new Add(),      new Subtract(),
                                           new Multiply(), new Divide()   )
    }

    class Sapling(tH: Int, fH: Int) extends Iterator[ExpNode] {
        def hasNext = true
        def rNode(): ExpNode = {
            if(tH <= 0) nodeSet.randomElement(nodeSet.terminals) //last layer
            else if (fH > 0) nodeSet.randomElement(nodeSet.nonterminals) //full layer
            else nodeSet.randomElement(nodeSet.elements) //above tH but below fH, any element goes
        }
        def next() = {
            rNode().buildUsing(new Sapling(tH-1, fH-1))
        }
    }

    class Tree(val t: ExpNode) extends Solution[Tree] {
        def inspect = t
        def eval(input: Seq[Double]): Seq[Double] = List(t.eval(input))
        lazy val fitness = fit(eval) + (parsimony * t.size)
        def mutate(): Tree = {
            val r = rand.nextDouble;
            if(r <= subtreeReplaceChance) {
                //replace random subtree
                val newSubtree = new Sapling(maxHeight,fullHeight).next()
                val toReplace  = rand.nextInt(t.size)
                new Tree(t.replaceSubtree(toReplace,newSubtree))
            } else {
                this
            }
        }
        def crossover(other: Tree): (Tree, Tree) = {
            val l = t
            val r = other.t
            val ( left, lidx) = nodeSet.getBiasedSubtree(l, crossoverBias)
            val (right, ridx) = nodeSet.getBiasedSubtree(r, crossoverBias)
            (new Tree(l.replaceSubtree(lidx, right)),
             new Tree(r.replaceSubtree(ridx, left) ) )
        }
        override def toString() = nodeSet.simplify(t).toString
    }
}
