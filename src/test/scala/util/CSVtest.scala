package com.util

import org.scalatest._

class CSVtest extends FlatSpec with Matchers {
    val csv = CSV.fromString(
        """
        # This is a comment line
        "a","b","c" # post line comment
        "1","2","3"""")
    "A CSV" should " be parsed from a string" in {
        assert( csv.titles == Seq("a","b","c") )
        assert( csv.rows == Seq(Seq("1","2","3")) )
    }
    it should " accept a new column" in {
        val result = csv.addColumn("d"){ row =>
            row("a") + row(2)
        }
        assert( result.titles == Seq("a","b","c","d") )
        assert( result.rows == Seq(Seq("1","2","3","13")))
    }
    it should " have filterable rows" in {
        val result = csv.filter{ row => false }
        assert( result.rows == Seq() )
    }
    it should " be iterable" in {
        var count = 0
        csv.foreach{ row =>
            count = count + 1
            assert( count == 1 )
            assert( row.values == Seq("1","2","3") )
        }
    }
    it should " be serializable" in {
        assert(csv.toString ==
            """|"a","b","c"
               |"1","2","3"""".stripMargin('|')
       )
    }
}
