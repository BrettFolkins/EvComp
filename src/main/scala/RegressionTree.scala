package com

import com.expTree._

import scala.util.Random

object RegressionTree {
    def apply(ds: DataSet, iHeight: Int, parsimony: Double): Problem = {

        val nodeSet = new Algebra {
            def randomConstantValue(): Double = ((rand.nextDouble()-0.5)*2.0)*ds.range
            def randomVarIndex() = rand.nextInt(ds.vectorLen)
            case class Variable(i: Int) extends ExpNode {
                def this() = this(randomVarIndex())
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

        class Tree(val t: ExpNode) extends Solution[Tree] {
            def inspect = t
            val fitness = {
                val ms = (for((data,target) <- ds.data) yield {
                    val ans  = t.eval(data)
                    val diff = target - ans
                    diff*diff
                }).sum
                val rms = Math.sqrt(ms)

                //rms
                rms + parsimony * t.size
                //(rms+1.0) * (1.0 + parsimony * t.size)
            }
            def mutate(): Tree = {
                //new Tree(nodeSet.mutateRandomNode(t))

                val r = nodeSet.rand.nextDouble;
/*                if(r <= 0.05) {
                    //replace random subtree
                    val toReplace  = nodeSet.rand.nextInt(t.size)
                    //val oldSubtree = t.pickSubtree(toReplace)
                    //val newDepth   = oldSubtree.depth / 2
                    val newSubtree = (new nodeSet.TreeGenerator(iHeight)).next()
                    new Tree(t.replaceSubtree(toReplace,newSubtree))
                } else */
                if (r <= 0.10) {
                    //become random subtree
                    new Tree(t.pickSubtree(nodeSet.rand.nextInt(t.size)))
                } else if (r <= 0.30) {
                    //mutate random node
                    new Tree(nodeSet.mutateRandomNode(t))
                } else {
                    //no mutation
                    this
                }
            }
            def crossover(other: Tree): (Tree, Tree) = {
                val l = t
                val r = other.t
                val ( left, lidx) = nodeSet.getBiasedSubtree(l, 0.8)
                val (right, ridx) = nodeSet.getBiasedSubtree(r, 0.8)
                (new Tree(l.replaceSubtree(lidx, right)),
                 new Tree(r.replaceSubtree(ridx, left) ) )

/*
                val (l,r) = nodeSet.crossoverSubtrees(t, other.t)
                (new Tree(l), new Tree(r))*/

            }
            override def toString() = t.toString + " size: " + t.size
        }

        new Problem {
            val generator = new nodeSet.TreeGenerator(iHeight)
            type SolutionType = Tree
            def potential() = new Tree(generator.next());
        }
    }

}
