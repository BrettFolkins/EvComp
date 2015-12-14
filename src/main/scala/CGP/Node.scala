package com.CGP

class Input
case class In(val index: Int) extends Input
case class Nd(val index: Int) extends Input

abstract class Node{
    def calc(get: Input => Double): Double
    def cons(child: () => Input): Node
    def children: Seq[Input]
    //print this operator applied to these strings
    def print(args: Seq[String]): String
}

class Constant(val gen: ()=>Double) extends Node {
    val value = gen()
    def children = Nil
    def calc(get: Input  => Double): Double = value
    def cons(child: () => Input): Node = new Constant(gen)
    def print(args: Seq[String]): String = toString()
    override def toString(): String = "%1.2f" format value
}

class UnaryNode(val op: (Double) => Double, sym: String, i: Input) extends Node{
    def children = List(i)
    def calc(get: Input => Double): Double = op(get(i))
    def cons(child: () => Input): Node = new UnaryNode(op, sym, child())
    def print(args: Seq[String]): String = sym+args.mkString("(", ",", ")")
    override def toString(): String = sym+"("+i+")"
}

class BinNode(val op: (Double, Double) => Double, sym: String,
                l: Input, r: Input) extends Node {
    def children = List(l,r)
    def calc(get: Input => Double): Double = op(get(l),get(r))
    def cons(child: () => Input): Node = new BinNode(op, sym, child(), child())
    def print(args: Seq[String]): String = args.mkString("(", sym, ")")
    override def toString(): String = "("+l+sym+r+")"
}

class NaryNode(
  val children: Seq[Input],
  numInput: Int,
  op: (Seq[Double]) => Double,
  name: String
) extends Node {
    def calc(get: Input  => Double): Double =
        op(children.map(get))
    def cons(child: () => Input): Node =
        new NaryNode((1 to numInput).map((x)=>child()), numInput, op, name)
    def print(args: Seq[String]): String = name+args.mkString("(", ",", ")")
    override def toString(): String = name+children.mkString("(", ",", ")")
}

object Node{
    implicit def BinaryOpWrapper( o: ((Double,Double)=>Double, String) ): Node =
        new BinNode(o._1, o._2, In(0), In(0))

    implicit def UnaryOpWrapper( o: ((Double)=>Double, String) ): Node =
        new UnaryNode(o._1, o._2, In(0))

    implicit def NaryOpWrapper( o: (Int, (Seq[Double]) => Double, String) ): Node =
        new NaryNode(Nil, o._1, o._2, o._3)

    def protectedDiv(n: Double, d: Double): Double = if(d != 0.0) n/d else n

    val algebraOps: Seq[Node] = List(((x:Double, y:Double)=>x+y , "+"),
                                     ((x:Double, y:Double)=>x*y , "*"),
                                     ((x:Double, y:Double)=>x-y , "-"),
                                     (protectedDiv(_,_) ,         "/") )
}
