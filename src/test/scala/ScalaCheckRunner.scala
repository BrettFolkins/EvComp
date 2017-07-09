package com

import com.scalacheck._

import org.scalacheck._

object ScalaCheckRunner {
  def main(args: Array[String]) = {
    ExpNodeTests.check()
    SimplifyTests.check()
  }
}
