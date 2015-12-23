package com

import com.graph._
import swing._

object GraphUtils {
    implicit def DataSourcePromoter(v: (String, Seq[Double])): DataSource =
        new ArrayDataSource(v._1, v._2)

    def chart(dataSets: DataSource*): Graph = {
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
            case ds: DataSet => {
                val correctData = ds.data.map(x => x._2)
                val foundData = for((data,target) <- ds.data) yield func(data)(0)
                chart( ("Correct", correctData.sorted), ("Found", foundData.sorted) )
            }
            case _ => chart(("No graph conversion found", Nil))
        }
    }

    class GraphWindow(val dataSets: Seq[DataSource]) extends SimpleSwingApplication{
        def top = new MainFrame{
            title = "Optimizer"
            contents = Component.wrap(chart(dataSets:_*))
        }
    }

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
