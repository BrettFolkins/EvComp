package com.expTree

import scala.util.Random

package object Algebra {

    val rand = new Random()

    def randomNode(h: Int): ExpNode = { //this will be abstract
        if(h > 1) Add(Constant(0.0),Constant(0.0))//random node
        else Constant(rand.nextDouble)
    }

    def getRandomSubtree(tree: ExpNode): ExpNode = {
        tree.pickSubtree(rand.nextInt(tree.size))
    }

    def mutateRandomNode(tree: ExpNode): ExpNode = {
        tree.mutateNode(rand.nextInt(tree.size), randomNode(0))
    }

    def crossoverSubtrees(left: ExpNode, right: ExpNode): (ExpNode, ExpNode) = {
        val li = rand.nextInt(left.size)
        val ri = rand.nextInt(right.size)
        ( left.replaceSubtree(li, right.pickSubtree(ri)),
         right.replaceSubtree(ri,  left.pickSubtree(li)) )
    }

    class TreeGenerator(height: Int) extends Iterator[ExpNode] {
        def hasNext = true
        def next() = {
            randomNode(height).buildUsing(new TreeGenerator(height-1))
        }
    }

    case class Constant(v: Double) extends ExpNode {
        val children = Nil
        def buildUsing(children: Iterator[ExpNode]) = Constant(v)
        def eval(i: Int) = v
        override def toString = v + "d"
    }

    implicit def ConstantPromoter(v: Double) = Constant(v)

    case class Add(l: ExpNode, r: ExpNode) extends ExpNode {
        val children = List(l, r)
        def buildUsing(subs: Iterator[ExpNode]) = {
            def sub(): ExpNode = if(subs.hasNext) subs.next() else 0.0
            Add(sub(), sub())
        }
        def eval(i: Int) = l.eval(i) + r.eval(i)
        override def toString = "("+l+"+"+r+")"
    }

    case class Subtract(l: ExpNode, r: ExpNode) extends ExpNode {
        val children = List(l, r)
        def buildUsing(subs: Iterator[ExpNode]) = {
            def sub(): ExpNode = if(subs.hasNext) subs.next() else 1.0
            Subtract(sub(), sub())
        }
        def eval(i: Int) = l.eval(i) - r.eval(i)
        override def toString = "("+l+"-"+r+")"
    }

    case class Multiply(l: ExpNode, r: ExpNode) extends ExpNode {
        val children = List(l, r)
        def buildUsing(subs: Iterator[ExpNode]) = {
            def sub(): ExpNode = if(subs.hasNext) subs.next() else 1.0
            Multiply(sub(), sub())
        }
        def eval(i: Int) = l.eval(i) * r.eval(i)
        override def toString = "("+l+"*"+r+")"
    }

    case class Divide(l: ExpNode, r: ExpNode) extends ExpNode {
        val children = List(l, r)
        def buildUsing(subs: Iterator[ExpNode]) = {
            def sub(): ExpNode = if(subs.hasNext) subs.next() else 1.0
            Divide(sub(), sub())
        }
        def eval(i: Int) = l.eval(i) / r.eval(i)
        override def toString = "("+l+"/"+r+")"
    }

 }
