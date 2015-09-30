package com.expTree

abstract class ExpNode extends Traversable[ExpNode]{

    def buildUsing(children: Iterator[ExpNode]): ExpNode

    val children: Seq[ExpNode]

    def terminal = children.isEmpty
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
    lazy val nodeCount: (Int, Int) = {
        if(terminal) (0, 1)
        else children.foldLeft((1, 0)){ case (s,c) =>
            val n = c.nodeCount
            (s._1 + n._1, s._2 + n._2)
        }
    }
    /**
     * Override size in Traversable to be an O(1) val
     */
    override lazy val size: Int = {
        val (nt, t) = nodeCount
        nt+t
    }
    /**
     * i is passed through for use by variables
     */
    def eval(i: Int): Double

    def eval(): Double = eval(0)

    def replaceChild(cidx: Int, child: ExpNode): ExpNode =
        buildUsing(children.updated(cidx, child).iterator)
    /**
     * runs as Preordered traversal of the tree from left to right
     */
    def foreach[U](op: ExpNode=>U): Unit = {
        op(this)
        children.foreach(c => c.foreach(op))
    }

    /**
     * Indexing below is based on preordered traversal visiting order
     * Since size is O(1), indexing can be done O(log(n)) for arbitrarily
     * constructed trees. Modifications create only O(log(n)) new nodes as well
     */

    def pickSubtree(id: Int): Option[ExpNode] = {
        if(id == 0) return Some(this)
        def findSubtree(i: Int, cs: Seq[ExpNode]): Option[ExpNode] = {
            if(cs.isEmpty) return None
            val total = cs.head.size
            if(i < total) cs.head.pickSubtree(i)
            else findSubtree(i-total, cs.tail)
        }
        findSubtree(id-1, children)
    }

    def indexOf(n: ExpNode): Option[Int] = {
        import scala.util.control.Breaks
        val b = new Breaks
        var index = 0;
        b.breakable {
            foreach{ x => if(x == n) b.break() else index += 1 }
            return None
        }
        Some(index)
    }

    def modifySubtree(id: Int, nn: ExpNode=>ExpNode): ExpNode = {
        if(id == 0) return nn(this)
        def findSubtree(i: Int, cs: Seq[ExpNode]): (ExpNode, Int) = {
            val total = cs.head.size
            if(i < total) {
                val subtree = cs.head.modifySubtree(i,nn)
                return (subtree, 0)
            }
            else {
                val (replaced, index) = findSubtree(i-total, cs.tail)
                return (replaced, index+1)
            }
        }
        val (replaced, index) = findSubtree(id-1, children)
        return replaceChild(index, replaced)
    }

    def mutateNode(id: Int, nn: ExpNode): ExpNode = {
        modifySubtree(id, (x: ExpNode) => {
            nn.buildUsing(x.children.iterator)
        })
    }

    def replaceSubtree(id: Int, nn: ExpNode): ExpNode = {
        modifySubtree(id, (x: ExpNode) => nn)
    }

    /**
     * Ideally, these would be expanded to the thouroghness of subtree selection
     * an easy way to avoid code duplication escapes me and, well, YAGNI
     */

    def pickTerminal(id: Int): Option[ExpNode] = {
        if(terminal){
            if(id == 0) return Some(this)
            else        return None
        }
        def findSubtree(i: Int, cs: Seq[ExpNode]): Option[ExpNode] = {
            if(cs.isEmpty) return None
            val (_, t) = cs.head.nodeCount
            if(i < t) cs.head.pickTerminal(i)
            else findSubtree(i-t, cs.tail)
        }
        findSubtree(id, children)
    }

    def pickNonTerminal(id: Int): Option[ExpNode] = {
        if(terminal) return None
        if(id == 0) return Some(this)
        def findSubtree(i: Int, cs: Seq[ExpNode]): Option[ExpNode] = {
            if(cs.isEmpty) return None
            val (nt, _) = cs.head.nodeCount
            if(i < nt) cs.head.pickNonTerminal(i)
            else findSubtree(i-nt, cs.tail)
        }
        findSubtree(id-1, children)
    }
}

