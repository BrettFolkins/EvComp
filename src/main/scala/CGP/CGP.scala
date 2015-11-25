package com.CGP

import java.util.Random
import com.FitnessEval
import com.Problem
import com.Solution

abstract class Node[T]{
    def calc(get: T  => Double): Double
    def cons(child: () => T): Node[T]
}

class Add(l: T, r: T) extends Node[T]{
    val chld = List(l, r)
    def calc(get: T  => Double): Double = chld.map(get(_)).reduce(_+_)
    def cons(child: () => T)): Node = new Add[T](child(), child())
    override def toString(): String = chld.mkString("(","+",")")
}

/*class Test(l: Input, r: Input) extends Node[Input]{
    val legs = List(l, r)
    def calc(get: Input => Double): Double = legs.map(get(_)).reduce(_+_)
    def cons(leg: ()    =>  Input): Node = new Test(leg(), leg())
    override def toString():String = legs.mkString("(",",",")")
}*/

class Input
case class In(val index: Int) extends Input
case class Nd(val index: Int) extends Input

/*
Node initialization
paramaterize mutation
paramaterize crossover
goal:
    list of first class functions translated to tree nodes
*/

class CGP(
  fit: FitnessEval,
  nodeset: Seq[Node],
  rows: Int,
  cols: Int = 1,
  depth: Int = Int.MaxValue
) extends Problem {
    type SolutionType = Grid
    val rand = new Random()

    def rIndex(): Int = rand.nextInt(rows*cols)

    def randomNode(index: Int): Node = {
        val row: Int = (index/cols)
        val dep: Int = Math.min(row, depth)
        def randInput() : Input = {
            val sel = rand.nextInt(fit.inputCount + dep*cols)
            if(sel < fit.inputCount) In(sel)
            else Nd(sel - fit.inputCount + (row-dep)*cols)
        }
        nodeset(rand.nextInt(nodeset.size)).cons(()=>randInput())
    }

    def potential(): Grid = new Grid( (0 until rows*cols) map (randomNode(_)) )

    class Grid(nodes: Seq[Node]) extends Solution[Grid] {
        val inspect = nodes
        lazy val fitness: Double = fit(evaluate(_))
        def evaluate(input: Seq[Double]): Seq[Double] = {
            val cache = Array.fill[(Boolean, Double)](nodes.size)((false, 0.0))
            def evalNode(i: Int): Double = {
                val v = nodes(i).calc(get)
                cache(i) = (true, v)
                v
            }
            def get(n: Input): Double = {
                n match {
                    case In(i) => input(i)
                    case Nd(i) => {
                        val v = cache(i)
                        if(v._1) v._2
                        else evalNode(i)
                    }
                }
            }
            val res = (nodes.size-fit.outputCount until nodes.size).map(evalNode(_))
            println(cache.grouped(cols).map(_.mkString("\t")).mkString("\n"))
            res
        }
        def mutate(): Grid = {
            //just node replacement for now
            val index = rIndex();
            new Grid(nodes.updated(index, randomNode(index)))
        }
        def crossover(other: Grid): (Grid, Grid) = {
            (this, other)
        }
        override def toString(): String =
            nodes.grouped(cols).map(_.mkString("\t")).mkString("\n")
    }
}

class TestFit extends FitnessEval{
    val range: Double = 10
    val inputCount: Int = 2
    val outputCount: Int = 2
    def apply(func: Seq[Double] => Seq[Double]) : Double = 0
}
