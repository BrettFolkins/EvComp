package com.expressions

import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe._

/**
 * Package for working with mathematical Seq[Double] => Double expressions
 */

class Algebra{

    val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()

    //symbol, print, precedence,
    //('+, tb.parse("com.expressions.Algebra.plus"))

    case class Const(v: Double) extends Expression{
        val children = Nil
        val precedence = 0
        val constant = true
        def buildUsing(children: Iterator[Expression]): Expression = this
        override def toString = "%1.2f".format(v)
    }

    case class Var(v: scala.Symbol) extends Expression{
        val children = Nil
        val precedence = 0
        val constant = false
        def buildUsing(children: Iterator[Expression]): Expression = this
        override def toString = v.toString
    }

    case class Func(t: tb.u.Tree, val children: Seq[Expression]) extends Expression{
        val precedence = 0
        val constant = false
        def buildUsing(c: Iterator[Expression]): Expression =
            Func(t, Seq(c.next(), c.next()))
        override def toString = children.mkString(t+"(",",",")")
    }

    val functions = Map( ('+, Func(tb.parse("com.expressions.Algebra.plus" ), Nil)),
                         ('*, Func(tb.parse("com.expressions.Algebra.times"), Nil)) )

    //make an expression using the operator corresponding to s
    def apply(s: scala.Symbol, c: Seq[Expression]): Expression = {
        if(c.isEmpty) Var(s)
        else functions(s).buildUsing(c.iterator)
    }

    def apply(d: Double): Expression = Const(d)

    //compile the expression to a native function
    def compile(e: Expression): (Map[scala.Symbol, Double]) => Double = {
        //list the `val name = varmap('name)` lines that will be needed
        def symVals(expr: Expression): Seq[tb.u.Tree] = {
            val syms = expr.flatMap(_ match {
                    case Var(s) => Seq(s.name)
                    case _ => Nil
                }).toSeq
            syms.map(s => tb.parse(s"""val $s = varmap(Symbol("$s"))"""))
        }

        def toAST(expr: Expression): tb.u.Tree = expr match {
            case Const(v) =>
                Literal(Constant(v))
            case Var(sym) =>
                Ident(TermName(sym.name))
            case Func(op, children) =>
                Apply(op, children.map(toAST).toList)
            case x => //take a wild guess
                tb.parse(x.toString)
        }

        val ast = q"""(varmap: Map[Symbol, Double]) => {
                        ..${symVals(e)}
                        ${toAST(e)}
                    }"""

        val compiled = tb.compile(ast)()
        compiled.asInstanceOf[Map[scala.Symbol, Double] => Double]
    }

/*    //return an expression in reduced terms
    def reduce(e: Expression): Expression = {

    }
*/
/*    def parse(s: String): Expression = {

    }
*/
    abstract class Expression extends IndexedTree[Expression]{
        val precedence: Int

        val constant: Boolean

        /**
         * This node should return a new node of the same operation, but using
         * the iterator to get references to new child nodes
         */
        def buildUsing(children: Iterator[Expression]): Expression

        // // // // // // // // // // // // // // // // // //

        def replaceChild(cidx: Int, child: Expression): Expression =
            buildUsing(children.updated(cidx, child).iterator)

        def modifySubtree(id: Int, nn: Expression=>Expression): Expression = {
            if(id == 0) return nn(this)
            def sid(rem: Int, cs: Seq[Expression], index: Int): (Expression, Int) = {
                if(cs.isEmpty) throw new IllegalArgumentException("Index out of bounds")
                val ns = cs.head.size
                if(rem < ns) (cs.head.modifySubtree(rem, nn), index)
                else sid(rem - ns, cs.tail, index + 1)
            }
            val (child, cidx) = sid(id-1, children, 0)
            replaceChild(cidx, child)
        }

        def mutateNode(id: Int, nn: Expression): Expression = {
            modifySubtree(id, (x: Expression) => {
                nn.buildUsing(x.children.iterator)
            })
        }

        def replaceSubtree(id: Int, nn: Expression): Expression = {
            modifySubtree(id, (x: Expression) => nn)
        }
    }
}

object Algebra {
    def plus (l: Double, r: Double):Double = l+r
    def times(l: Double, r: Double):Double = l*r
}
