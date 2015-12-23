package com.ml

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
