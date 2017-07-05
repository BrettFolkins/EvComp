package com.util

import com.ml.DataSet
import com.ml.FitnessEval
import com.graph._
import swing._

object Chart {
    implicit def DataSourcePromoter(v: (String, Seq[Double])): DataSource =
        new ArrayDataSource(v._1, v._2)

    def apply(dataSets: DataSource*): Graph = {
        val aL = new java.util.ArrayList[DataSource]()
        for(ds <- dataSets) aL.add(ds)
        new Graph(aL, true)
    }

    abstract class FitnessEvalwShow extends FitnessEval{
        def show(x: Seq[Double] => Seq[Double]): Graph
    }

    def show(fit: FitnessEval, func: Seq[Double] => Seq[Double]) = {
        fit match {
            case fe: FitnessEvalwShow => {
                fe.show(func)
            }
            case _ => Chart(("No graph conversion found", Nil))
        }
    }

    class ChartWindow(val graph: Graph) extends SimpleSwingApplication{
        def top = new MainFrame{
            title = "Optimizer"
            contents = Component.wrap(graph)
        }
    }
    def ChartWindow(dataSets: Seq[DataSource]) =
        new ChartWindow(Chart(dataSets:_*))

    class ArrayDataSource(val getName: String, data: Seq[Double]) extends DataSource {
        def get(X: Double): Double = {
            if(data.isEmpty) return 0.0
            else {
                val index :Int = (X*data.size.toDouble).toInt
                data(index).toDouble
            }
        }
    }
}
