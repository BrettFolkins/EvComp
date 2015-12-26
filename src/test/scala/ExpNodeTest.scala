import org.scalacheck._

import com.ml.expTree._
import com.ml.expTree.BasicAlgebra._
import scala.util.Random

object ExpNodeTests extends Properties("expTree.ExpNode") {
    import Prop.{forAll, BooleanOperators}

    val treeGen = for { n <- Gen.choose(1,4) } yield (new TreeGenerator(n)).next()
    implicit val arbTree = Arbitrary(treeGen)

    val rand = new Random()

    property("Mutation invariant") = forAll { (e: ExpNode) =>
        val before = e.eval()
        val mutatedTree = mutateRandomNode(e)
        val after = e.eval()
        before == after
    }

    property("PickTerminal index calculation") = forAll { (e: ExpNode) =>
        val (node, index) = e.pickTerminal(rand.nextInt(e.nodeCount._2))
        node == e.pickSubtree(index)
    }

    property("PickNonTerminal index calculation") = forAll { (e: ExpNode) =>
        e.nodeCount._1 > 0 ==> {
            val (node, index) = e.pickNonTerminal(rand.nextInt(e.nodeCount._1))
             node == e.pickSubtree(index)
        }
    }

    property("Identity Mutation") = forAll { (e: ExpNode) =>
        e.nodeCount._1 > 0 ==> {
            val (node, index) = e.pickNonTerminal(rand.nextInt(e.nodeCount._1))
            val ident = e.mutateNode(index, node)
            e.eval() == ident.eval()
        }
    }
}
