package com

import scala.util.Random

/**
 * Put all this in an expression set, so they can all reference the sets
 * factories and bounds?
 */

package object expTree {
    abstract class ExpNode {
        //not sure that I like this
        def buildUsing(children: Iterator[ExpNode]): ExpNode

        val children: Seq[ExpNode]

        lazy val terminal = children.isEmpty
        /**
         * lazy because it depends on an abstract member that will be
         * initialized after this constructor is called
         */
        lazy val depth: Int = if(children.isEmpty) 1
                              else 1 + (children map (x => x.depth)).max
        /**
         * lazy because it depends on an abstract member
         * returns a pair (nonTermianls, Terminals)
         */
        lazy val size: (Int, Int) = {
            if(terminal) (0, 1)
            else children.foldLeft((1, 0)){ case (s,c) =>
                val n = c.size
                (s._1 + n._1, s._2 + n._2)
            }
        }
        /**
         * return the evaluated expression
         * i is for passed through for use by variables
         */
        def eval(i: Int): Double
        /**
         * Return a node of the same model as this node, but
         * with one child replaced
         */
        def replaceChild(cidx: Int, child: ExpNode): ExpNode =
            buildUsing(children.updated(cidx, child).iterator)
    }

    /**
     * Stores the information necessary to replace a node in a tree
     * the first element is the node to be replaced
     * the second element is the node the first element is a child of,
     *      and the first element's index in the list of children
     * and so on ...
     */
    type treePos = List[(ExpNode, Int)]

    def rebuild(tP: treePos, nn: ExpNode): ExpNode = {//build from new node and treePos

    }

    def getRandomPoint(node: ExpNode): ExpNode = {
        /**
         * call size of the node to get the total elements
         * do the thing where one of N elements is chosen moving sequentially
         * but for like, groups or whatever
         */
    }
    //what does the implementer provide to specify random node selection minially?

    class TreeGenerator(height: Int) extends Iterator[ExpNode] {
        def hasNext = true
        def next() = {
            randomNode(height).buildUsing(new TreeGenerator(height-1))
        }
    }

    //pick random terminal
    //pick random nonterminal
    //mutate tree
    //crossover trees
    //node building from nothing

    //apps extend expTree, provide terminal and op set
    //algebra set extension should come wih +,-,*,/

    val nodes = Set(Constant, Add, Multiply)

    val rand = new Random()
    def randomNode(h: Int): ExpNode = { //this will be abstract
        if(h > 1) Add(Constant(0.0),Constant(0.0))//random node
        else Constant(rand.nextDouble)
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
        def buildUsing(children: Iterator[ExpNode]) = {
            def getChild(i: Int) = {
                if(children.hasNext) children.next()
                else Constant(0.0)
            }
            Add(getChild(0), getChild(1))
        }
        def eval(i: Int) = l.eval(i) + r.eval(i)
        override def toString = "("+l+"+"+r+")"
    }

    case class Multiply(l: ExpNode, r: ExpNode) extends ExpNode {
        val children = List(l, r)
        def buildUsing(children: Iterator[ExpNode]) = {
            def getChild(i: Int) = {
                if(children.hasNext) children.next()
                else Constant(1.0)
            }
            Multiply(getChild(0), getChild(1))
        }
        def eval(i: Int) = l.eval(i) * r.eval(i)
        override def toString = "("+l+"*"+r+")"
    }


 }

