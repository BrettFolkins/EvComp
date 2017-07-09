package com.scalacheck

import org.scalacheck._

import com.ml.expTree._
import com.ml.expTree.BasicAlgebra._

object SimplifyTests extends Properties("ExpTree.Simplify") {
    import Prop.{forAll, BooleanOperators}

    val treeGen = for { n <- Gen.choose(1,4) } yield (new TreeGenerator(n)).next()
    implicit val arbTree = Arbitrary(treeGen)

    property("ConstantAdd") = forAll { (a: Double, b: Double) =>
        simplify(Add(Constant(a), Constant(b))) == Constant(a+b)
    }

    property("ConstantSubtract") = forAll { (a: Double, b: Double) =>
        simplify(Subtract(Constant(a), Constant(b))) == Constant(a-b)
    }

    property("ConstantMult") = forAll { (a: Double, b: Double) =>
        simplify(Multiply(Constant(a), Constant(b))) == Constant(a*b)
    }

    property("ConstantDiv") = forAll { (a: Double, b: Double) => (b != 0) ==>
        (simplify(Divide(Constant(a), Constant(b))) == Constant(a/b))
    }

    property("Simplification maintains final result") = forAll { (e: ExpNode) =>
        e.eval() == simplify(e).eval()
    }

}
