package finalStep

import JaCoP.scala._
import scala.io.Source

object Test extends jacop {

  def main(args: Array[String]): Unit = {
    val t = Source.fromFile("corequis.txt").getLines
    val list = (for(a <- t) yield List(a.split(";")(0), a.split(";")(1))).toList
    
    val list2 = for(a <- list) println(a)
  }
} 