package com.ml

import scala.collection.mutable.Buffer

/**
  * Diagnostics are passed to Optimizers, which use the Diagnostic object
  * to pass information about progress back up to the calling code.
  * Optimizers also have the option of respecting the "finished" field of
  * a diagnostic object to complete early
  */
trait Diagnostic[-T] {
    def log(popualation: Seq[T]) : Unit
    def finished : Boolean = false
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
