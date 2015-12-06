package com.CGP

//import java.util.concurrent.ThreadLocalRandom
import com.Entropy.rand
import com.FitnessEval
import com.Problem
import com.Solution

class CGP(
  fit: FitnessEval,
  nodeset: Seq[Node],
  rows: Int,
  cols: Int = 1,
  depth: Int = Int.MaxValue,
  mutateChance: Double = 0.05
) extends Problem {
    type SolutionType = Grid

    //def rand = ThreadLocalRandom.current()

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

    class Grid(val nodes: Seq[Node]) extends Solution[Grid] {
        val inspect = nodes
        lazy val fitness: Double = fit(eval(_)._1)

        def eval(input: Seq[Double]): (Seq[Double], Seq[(Boolean, Double)]) = {
            val cache = Array.fill[(Boolean, Double)](nodes.size)((false, 0.0))
            def evalNode(i: Int): Double = {
                val v = nodes(i).calc(get)
                cache(i) = (true, v)
                v
            }
            def get(n: Input): Double = n match {
                case In(i) => input(i)
                case Nd(i) => {
                    val v = cache(i)
                    if(v._1) v._2
                    else evalNode(i)
                }
            }

            val res = (nodes.size-fit.outputCount until nodes.size).map(evalNode(_))
            //println(cache.grouped(cols).map(_.mkString("\t")).mkString("\n"))
            (res, cache)
        }

        def mutate(): Grid = CGP.this.mutate(this)

        def crossover(other: Grid): (Grid, Grid) = CGP.this.crossover(this, other)

        def showGrid: String =
            nodes.grouped(cols).map(_.mkString("\t")).mkString("\n")

        def showEquation: String = {
            val (_,used) = eval((1 to fit.inputCount).map(_.toDouble))
            val (extrons,_) = nodes.zipWithIndex.filter{ case (_, i) => used(i)._1 }.unzip
            //extrons.mkString("\n")

            //filter to used Nodes's
            //Nodes's used more than once get a name
            //Nodes's used once becomes its print(...) string
            //print nodes used more than once's print(...)

            val usedInputs = extrons.flatMap(_.children)
            val usedNds = usedInputs.filter(_.isInstanceOf[Nd])
            val usedNodes = usedNds.map{ case Nd(x) => nodes(x) }

            val plural = extrons.filter(n => usedNodes.count(_==n) > 1)

            def prettyName(i: Int): String = ('A'.toInt+i).toChar.toString
            val names = Map[Node,String](
                plural.zipWithIndex.map{case (n,i) => (n -> prettyName(i))}:_*)

            def build(n: Node): String = n.print(n.children.map(inputString(_)))
            def nodeString(n: Node): String = names.getOrElse(n, build(n))
            def inputString(i: Input): String = i match {
                case In(x) => "X"+x
                case Nd(x) => nodeString(nodes(x))
            }

            val subExprs = plural.map(n => names(n)+": "+build(n))
            val outputs = nodes.takeRight(fit.outputCount).map(n => build(n))
            (subExprs++:outputs).mkString("\n")
        }

        override def toString(): String = showEquation
    }

    def potential(): Grid = new Grid( (0 until rows*cols) map (randomNode(_)) )

    def mutate(o: Grid): Grid =
        replaceNodes(()=>rand.nextDouble<=mutateChance)(o)

    def crossover(a: Grid, b: Grid): (Grid, Grid) = {
        val (l,r) = {
            val l = rand.nextInt(rows*cols+1)
            val r = rand.nextInt(rows*cols+1)
            if(l > r) (r, l)
            else (l, r)
        }
        val diff = r-l
        val an = a.nodes
        val bn = b.nodes
        (new Grid(an.patch(l, bn.view(l,r), diff)),
         new Grid(bn.patch(l, an.view(l,r), diff)) )
    }

    def replaceNode(o: Grid): Grid = {
        val index = rIndex();
        new Grid(o.nodes.updated(index, randomNode(index)))
    }

    def replaceNodes(c:()=>Boolean)(o: Grid): Grid = {
        new Grid(o.nodes.zipWithIndex.map{ case (n, i) =>
            if(c()) randomNode(i)
            else n
        })
    }
}

class TestFit extends FitnessEval{
    val range: Double = 10
    val inputCount: Int = 2
    val outputCount: Int = 2
    def apply(func: Seq[Double] => Seq[Double]) : Double = 0
}

