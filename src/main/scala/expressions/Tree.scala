package com.expressions

abstract class IndexedTree[Tree <: IndexedTree[Tree]] extends Traversable[IndexedTree[Tree]] {

    val children: Seq[Tree]

    lazy val terminal = children.isEmpty

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

    //default traversable uses foreach in toString, causes infinite loop
    override def toString(): String = children.mkString("Tree(",",",")")

    /**
     * Indexing below is based on preordered traversal visiting order
     * Since size is O(1), indexing can be done O(log(n)) for arbitrarily
     * constructed trees. Modifications create only O(log(n)) new nodes.
     */

    def foreach[U](op: IndexedTree[Tree] => U): Unit = {
        op(this)
        children.foreach(c => c.foreach(op))
    }

    def find(n: Tree): Option[Int] = {
        var index = 0;
        foreach{ x:IndexedTree[Tree] =>
            if(x == n) return Some(index)
            else index += 1
        }
        None
    }

    def pickSubtree(id: Int): IndexedTree[Tree] = {
        if(id == 0) return this
        def findSubtree(i: Int, cs: Seq[Tree]): IndexedTree[Tree] = {
            if(cs.isEmpty) throw new IllegalArgumentException("Index out of bounds")
            val total = cs.head.size
            if(i < total) cs.head.pickSubtree(i)
            else findSubtree(i-total, cs.tail)
        }
        findSubtree(id-1, children)
    }

    /**
     * Pick Terminal/nonTerminal returns an Tree reference and the
     * full tree index of the chosen node
     */

    def pickTerminal(id: Int): (IndexedTree[Tree], Int) = {
        if(terminal){
            if(id == 0) return (this,0)
            else        throw new IllegalArgumentException("Index out of bounds")
        }
        def findSubtree(i: Int, cs: Seq[Tree]): (IndexedTree[Tree], Int) = {
            if(cs.isEmpty) throw new IllegalArgumentException("Index out of bounds")
            val (nt, t) = cs.head.nodeCount
            //get the node, its position in the subree, and the subtree's position
            val ((node, pos), d) = {
                if(i < t) (  cs.head.pickTerminal(i), 1)
                else      (findSubtree(i-t, cs.tail), nt+t)
            }
            (node, pos+d)
        }
        findSubtree(id, children)
    }

    def pickNonTerminal(id: Int): (IndexedTree[Tree], Int) = {
        if(terminal) throw new IllegalArgumentException("Nonterminal search on terminal element")
        if(id == 0) return (this, 0)
        def findSubtree(i: Int, cs: Seq[Tree]): (IndexedTree[Tree], Int) = {
            if(cs.isEmpty) throw new IllegalArgumentException("Index out of bounds")
            val (nt, t) = cs.head.nodeCount
            //get the node, its position in the subree, and the subtree's position
            val ((node, pos), d) = if(i < nt) (cs.head.pickNonTerminal(i), 1)
                                   else       (findSubtree(i-nt, cs.tail), nt+t)
            (node, pos+d)
        }
        findSubtree(id-1, children)
    }
}

