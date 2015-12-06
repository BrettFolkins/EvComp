package com.expTree

import com.Entropy.rand

abstract class Algebra {
    val elements: Seq[ExpNode]

    def randomConstantValue(): Double

    lazy val (terminals, nonterminals) = elements.partition(x => x.terminal)

    def randomElement[U](xs: Seq[U]): U = xs(rand.nextInt(xs.size))

    def getRandomSubtree(tree: ExpNode): ExpNode = {
        tree.pickSubtree(rand.nextInt(tree.size))
    }

    def getBiasedSubtree(tree: ExpNode, bias: Double): (ExpNode, Int) = {
        if(tree.terminal) return (tree,0)
        if(rand.nextDouble > bias) {
            tree.pickTerminal(rand.nextInt(tree.nodeCount._2))
        } else {
            tree.pickNonTerminal(rand.nextInt(tree.nodeCount._1))
        }
    }

    def mutateRandomNode(tree: ExpNode): ExpNode = {
        tree.mutateNode(rand.nextInt(tree.size), randomElement(elements))
    }

    def crossoverSubtrees(left: ExpNode, right: ExpNode): (ExpNode, ExpNode) = {
        val li = rand.nextInt(left.size)
        val ri = rand.nextInt(right.size)
        ( left.replaceSubtree(li, right.pickSubtree(ri)),
         right.replaceSubtree(ri,  left.pickSubtree(li)) )
    }

    class TreeGenerator(height: Int) extends Iterator[ExpNode] {
        def hasNext = true

        def randomNode(below: Int): ExpNode = {
            if(below > 0) randomElement(elements)
            else randomElement(terminals)
        }

        def next() = {
            randomNode(height).buildUsing(new TreeGenerator(height-1))
        }
    }

    case class Constant(v: Double) extends ExpNode {
        override val constant = true
        def this() = this(0.0)
        val children = Nil
        def buildUsing(children: Iterator[ExpNode]) = Constant(randomConstantValue())
        def eval(i: Any) = v
        override def toString = "%1.2f" format v
    }

    implicit def ConstantPromoter(v: Double) = Constant(v)

    case class Add(l: ExpNode, r: ExpNode) extends ExpNode {
        override val constant = l.constant && r.constant
        def this() = this(0.0,0.0)
        val children = List(l, r)
        def buildUsing(subs: Iterator[ExpNode]) = {
            def sub(): ExpNode = if(subs.hasNext) subs.next() else 0.0
            Add(sub(), sub())
        }
        def eval(i: Any) = l.eval(i) + r.eval(i)
        override def toString = "("+l+"+"+r+")"
    }

    case class Subtract(l: ExpNode, r: ExpNode) extends ExpNode {
        override val constant = l.constant && r.constant
        def this() = this(0.0,0.0)
        val children = List(l, r)
        def buildUsing(subs: Iterator[ExpNode]) = {
            def sub(): ExpNode = if(subs.hasNext) subs.next() else 1.0
            Subtract(sub(), sub())
        }
        def eval(i: Any) = l.eval(i) - r.eval(i)
        override def toString = "("+l+"-"+r+")"
    }

    case class Multiply(l: ExpNode, r: ExpNode) extends ExpNode {
        override val constant = l.constant && r.constant
        def this() = this(1.0,1.0)
        val children = List(l, r)
        def buildUsing(subs: Iterator[ExpNode]) = {
            def sub(): ExpNode = if(subs.hasNext) subs.next() else 1.0
            Multiply(sub(), sub())
        }
        def eval(i: Any) = l.eval(i) * r.eval(i)
        override def toString = "("+l+"*"+r+")"
    }

    case class Divide(l: ExpNode, r: ExpNode) extends ExpNode {
        override val constant = l.constant && r.constant
        def this() = this(1.0,1.0)
        val children = List(l, r)
        def buildUsing(subs: Iterator[ExpNode]) = {
            def sub(): ExpNode = if(subs.hasNext) subs.next() else 1.0
            Divide(sub(), sub())
        }
        def eval(i: Any) = {
            val reval = r.eval(i)
            val leval = l.eval(i)
            if(reval == 0) leval
            else leval/reval
        }
        override def toString = "("+l+"/"+r+")"
    }

    def simplify(t: ExpNode): ExpNode = {
        if(t.terminal) return t

        val left  = simplify(t.children(0))
        val right = simplify(t.children(1))

        t match {
            case a: Add => {
                if(left.constant && right.constant){
                    Constant(left.eval() + right.eval())
                } else if (left == Constant(0.0)) {
                    right
                } else if (right == Constant(0.0)) {
                    left
                } else {
                    Add(left, right)
                }
            }
            case a: Multiply => {
                if(left.constant && right.constant){
                    Constant(left.eval() * right.eval())
                } else if (left == Constant(1.0)) {
                    right
                } else if (right == Constant(1.0)) {
                    left
                } else {
                    Multiply(left, right)
                }
            }
            case a: Divide => {
                if(right == Constant(0.0)) {
                    left
                } else if(left.constant && right.constant){
                    Constant(left.eval() / right.eval())
                } else if (left == Constant(0.0)) {
                    Constant(0.0)
                } else if (right == Constant(1.0)) {
                    left
                } else if (left == right) {
                    Constant(1.0)
                } else {
                    Divide(left, right)
                }
            }
            case a: Subtract => {
                if(left.constant && right.constant){
                    Constant(left.eval() - right.eval())
                } else if (right == Constant(0.0)) {
                    left
                } else if (left == right) {
                    Constant(0.0)
                } else {
                    Subtract(left, right)
                }
            }
            case _ => t
        }
    }

}

object BasicAlgebra extends Algebra {
    val elements: Seq[ExpNode] = Array(new Constant(),
                                       new Add(), new Subtract(),
                                       new Multiply(), new Divide())

    def randomConstantValue(): Double = rand.nextInt(10).toDouble
}
