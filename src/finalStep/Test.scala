package finalStep

import JaCoP.scala._
import scala.io.Source

object Test extends jacop {

  def main(args: Array[String]): Unit = {
    val t = Source.fromFile("cours.txt").getLines
    while (t.hasNext) {
      val a = t.next.split(";")
      println(a(0) + " 0 " + a(1))
    }
  }
} 