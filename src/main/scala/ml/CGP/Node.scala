package com.ml.CGP

class Input
case class In(val index: Int) extends Input
case class Nd(val index: Int) extends Input

abstract class Node{
    def calc(get: Input => Seq[Double]): Seq[Double]
    def children: Seq[Input]
    //print this operator applied to these strings
    def print(args: Seq[String]): String
}

class Constant(val value: Double) extends Node {
    def children = Nil
    def calc(get: Input  => Seq[Double]): Seq[Double] = get(In(0)).map{_ => value}
    def print(args: Seq[String]): String = toString()
    override def toString(): String = "%1.2f" format value
}

class UnaryNode(val op: (Double) => Double, sym: String, i: Input) extends Node{
    def children = List(i)
    def calc(get: Input => Seq[Double]): Seq[Double] = get(i).map{ op(_) }
    def print(args: Seq[String]): String = sym+args.mkString("(", ",", ")")
    override def toString(): String = sym+"("+i+")"
}

class BinNode(val op: (Double, Double) => Double, sym: String,
                l: Input, r: Input) extends Node {
    def children = List(l,r)
    def calc(get: Input => Seq[Double]): Seq[Double] =
        get(l).zip(get(r)).map{ case (l,r) => op(l,r) }
    def print(args: Seq[String]): String = args.mkString("(", sym, ")")
    override def toString(): String = "("+l+sym+r+")"
}

class NaryNode(
  val children: Seq[Input],
  numInput: Int,
  op: (Seq[Double]) => Double,
  name: String
) extends Node {
    def calc(get: Input  => Seq[Double]): Seq[Double] = {
        val inputBlocks = children.map{ get(_) }
        val count = inputBlocks(0).size
        (0 until count).map{ i => op(inputBlocks.map{ c => c(i) }) }
    }
    def print(args: Seq[String]): String = name+args.mkString("(", ",", ")")
    override def toString(): String = name+children.mkString("(", ",", ")")
}

trait NodeGen{
    def generate(arg: () => Input): Node
}

object Node{
    implicit def UnaryOpWrapper( o: ((Double)=>Double, String) ) =
        new NodeGen {
            def generate(child: () => Input) =
                new UnaryNode(o._1, o._2, child())
        }

    implicit def BinaryOpWrapper( o: ((Double,Double)=>Double, String) ) =
        new NodeGen {
            def generate(child: () => Input) =
                new BinNode(o._1, o._2, child(), child())
        }

    implicit def NaryOpWrapper( o: (Int, (Seq[Double]) => Double, String) ) =
        new NodeGen {
            def generate(child: () => Input) = {
                val children: Seq[Input] = (1 to o._1).map{ _ => child() }
                new NaryNode(children, o._1, o._2, o._3)
            }
        }

    def protectedDiv(n: Double, d: Double): Double = if(d != 0.0) n/d else n
    def protectedLog(x: Double): Double = if(x != 0.0) Math.log(Math.abs(x)) else 0.0
    def protectedExp(x: Double): Double = if(Math.abs(x) >= 1.0) x else Math.exp(x)

    val algebraOps: Seq[NodeGen] = List(((x:Double, y:Double)=>x+y , "+"),
                                     ((x:Double, y:Double)=>x*y , "*"),
                                     ((x:Double, y:Double)=>x-y , "-"),
                                     (protectedDiv(_,_) ,         "/") )

    val exponentialOps: Seq[NodeGen] = List(
        (protectedExp(_), "e^"),
        (protectedLog(_) , "ln") )

    def constantInRange(range: Double): NodeGen = constantInRange(-range, range)
    def constantInRange(min: Double, max: Double): NodeGen = {
        new NodeGen{
            def rnd(): Double = {
                import com.util.Entropy.rand
                rand.nextDouble*(max-min) + min
            }
            def generate(child: () => Input) = new Constant(rnd())
        }
    }

    def algebra(range: Double): Seq[NodeGen] = {
        algebraOps :+ constantInRange(range)
    }
}
