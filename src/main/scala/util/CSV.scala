package com.util

import scala.util.matching._

class CSV(val titles: Seq[String], val rows: Seq[Seq[String]]) {
    class Row(val values: Seq[String]){
        def apply(s: String) = titles.indexOf(s) match {
            case -1 => throw new Exception("Unknown column title: "+s)
            case  i => values(i)
        }
        def apply(i: Int) = values(i)
    }
    /**
     * Return a new CSV with the new column appended
     * each columns value is the result of the `op` function applied to the row
     *   it will be appended to
     */
    def addColumn(title: String)(op: Row => String): CSV = {
        new CSV(titles :+ title, rows.map { r => r :+ op(new Row(r)) })
    }
    /**
     * Filter select lines out of the CSV
     */
    def filter(op: Row => Boolean): CSV = {
        new CSV(titles, rows.filter { r => op(new Row(r)) })
    }
    /**
     * Apply an operation to each row in the CSV file
     */
    def foreach(op: Row => Unit): CSV = {
        rows.foreach { r => op(new Row(r)) }
        this
    }
    /**
     * Serialize to a CSV file format
     * note that comments are not retained
     */
    override def toString: String =
        (titles +: rows)
            .map { r => r.mkString("\"","\",\"","\"") }
            .mkString("\n")
}

object CSV {
    val quotedCSV = "\"([^\"]*)\"".r
    val rejectComments = "[^#]*\"[^#]*".r
    /**
     * CSV line format:
     * each value is wrapped by quotation marks and seperated by a comma
     * any text after a '#' on each line is ignored
     * lines that are empty or don't contain any quotes are ignored
     */
    def fromString(data: String): CSV = {
        val raw = data
            .split("\n")
            .flatMap { l => rejectComments.findPrefixOf(l) }
            .map { l =>
                quotedCSV.findAllMatchIn(l)
                         .map { _.group(1) }
                         .toList
            }
        new CSV(raw.head, raw.tail)
    }
}
