package com

import scala.collection.mutable.Buffer

trait Diagnostic[-T] {
    def log(popualation: Seq[T]) : Unit
}

object Diagnostic{
    class noop extends Diagnostic[Any] {
        def log(pop: Seq[Any]) {}
    }
    implicit val emptyDiagnostic = new noop

    def average(store: Buffer[Double]) = new Diagnostic[Solution[_]]{
        def log(pop: Seq[Solution[_]]) {
            store += pop.map(x => x.fitness).sum / pop.size.toDouble
        }
    }

    def best(store: Buffer[Double]) = new Diagnostic[Solution[_]]{
        def log(pop: Seq[Solution[_]]) {
            store += pop.map(x => x.fitness).min
        }
    }
}

trait Optimizer {
    /**
     * Given a Problem, an optimizer well return the best solution it finds
     * and give a Diagnostic the chance to inspect the population
     */
    def apply(p: Problem)(implicit ds: Diagnostic[p.SolutionType]): (p.SolutionType)
}
