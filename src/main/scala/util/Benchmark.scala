package com.util

object Benchmark{
    def time[T](func: => T): (T, Double) = {
        val start   = System.nanoTime()
        val rtn     = func
        val end     = System.nanoTime()
        val seconds = (end-start).toDouble / 1000000000d;
        (rtn, seconds)
    }
}
