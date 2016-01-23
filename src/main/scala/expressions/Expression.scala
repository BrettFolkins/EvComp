package com.expressions

import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe._

/**
 * Package for working with mathematical Map[Symbol, Double] => Double expressions
 */

class Algebra{
    val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()

    case class Const(v: Double) extends Expression{
        val children = Nil
        val constant = true
        def buildUsing(children: Seq[Expression]): Expression = this
        override def toString = "%1.2f".format(v)
    }

    case class Var(v: scala.Symbol) extends Expression{
        val children = Nil
        val constant = false
        def buildUsing(children: Seq[Expression]): Expression = this
        override def toString = v.toString
    }

    case class Op(t: Tree, val children: Seq[Expression]) extends Expression{
        def this(t: Tree) { this(t, Nil) }
        val constant = children.forall(_.constant)
        def buildUsing(c: Seq[Expression]): Expression = Op(t, c)
        override def toString = children.mkString(t+"(",",",")")
    }

    val functions = Map(
            ('+ -> new Op(q"com.expressions.Algebra.plus"    )),
            ('* -> new Op(q"com.expressions.Algebra.times"   )),
            ('- -> new Op(q"com.expressions.Algebra.subtract")),
            ('/ -> new Op(q"com.expressions.Algebra.divide"  ))
        )

    //make an expression using the operator corresponding to s
    def apply(s: scala.Symbol, c: Seq[Expression]): Expression =
        functions(s).buildUsing(c)

    def apply(t: Tree, c: Seq[Expression]): Expression = Op(t, c)

    def apply(s: scala.Symbol): Expression = Var(s)

    def apply(d: Double): Expression = Const(d)

    //compile the expression to a native function
    def compile(e: Expression): (Map[scala.Symbol, Double]) => Double = {
        //list the `val name = varmap('name)` lines that will be needed
        def symVals(expr: Expression): Seq[tb.u.Tree] = {
            val syms = expr.flatMap(_ match {
                    case Var(s) => Seq(s.name)
                    case _ => Nil
                }).toSeq.distinct
            syms.map(s => tb.parse(s"""val ${"v"+s} = varmap(Symbol("$s"))"""))
        }

        def toAST(expr: Expression): tb.u.Tree = expr match {
            case Const(v) =>
                Literal(Constant(v))
            case Var(sym) =>
                Ident(TermName("v"+sym.name))
            case Op(op, children) =>
                Apply(op, children.map(toAST).toList)
        }

        val ast = q"""(varmap: Map[Symbol, Double]) => {
                        ..${symVals(e)}
                        ${toAST(e)}
                    }"""

        val compiled = tb.compile(ast)()
        compiled.asInstanceOf[Map[scala.Symbol, Double] => Double]
    }

/*    def reduce(e: Expression): Expression = {

    }
*/
/*    def parse(s: String): Expression = {

    }
*/
/*    def pretty(e: Expression): String = {

    }
*/

    abstract sealed class Expression extends IndexedTree[Expression]{
        val constant: Boolean

        /**
         * This node should return a new node of the same operation, but using
         * the iterator to get references to new child nodes
         */
        def buildUsing(children: Seq[Expression]): Expression

        // // // // // // // // // // // // // // // // // //

        def replaceChild(cidx: Int, child: Expression): Expression =
            buildUsing(children.updated(cidx, child))

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
                nn.buildUsing(x.children)
            })
        }

        def replaceSubtree(id: Int, nn: Expression): Expression = {
            modifySubtree(id, (x: Expression) => nn)
        }
    }
}

object Algebra {
    def plus    (l: Double, r: Double): Double = l+r
    def times   (l: Double, r: Double): Double = l*r
    def subtract(l: Double, r: Double): Double = l-r
    def divide  (l: Double, r: Double): Double =
        if(r == 0.0 || r.isInfinite() || r.isNaN()) l else l/r
}
