import org.scalacheck._

import com.expTree._
import com.expTree.BasicAlgebra._

object ExpNodeTests extends Properties("expTree.ExpNode") {
    import Prop.{forAll, BooleanOperators}

    val treeGen = for { n <- Gen.choose(1,4) } yield (new TreeGenerator(n)).next()
    implicit val arbTree = Arbitrary(treeGen)

    property("Mutation invariant") = forAll { (e: ExpNode) =>
        val before = e.eval()
        val mutatedTree = mutateRandomNode(e)
        val after = e.eval()
        before == after
    }
}
