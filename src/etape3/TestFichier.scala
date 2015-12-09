package etape3



object TestFichier {
  import java.io.File
  import scala.io.Source
  
  case class CsvFile(headers:Option[Seq[String]],lines:Seq[Seq[String]]) {
    //don't do that on large csv...
    override def toString = {
      if(lines.isEmpty) ""
      else {
        headers.map(_.mkString(",")).getOrElse("") +
        lines.map( _.mkString(",") ).mkString("\n")
      }
    }
  }
  
  
  def csvParser(path:String, hasHeader:Boolean = true, skipLines : Int = 0)(implicit separator:Char = ',') : CsvFile = {
    def parseLine(line:String) : Seq[String] = line.split(separator).toSeq
    
    val file = new File(path)
    require(file.exists && file.isFile && file.canRead, "'%s' is not a correct file for parsing".format(path))
    
    val lines = Source.fromFile(file).getLines.drop(skipLines).map(parseLine _).toSeq
    
    if(lines.size < 1) {
      CsvFile(None,Seq())
    } else {
      if(hasHeader) {
        CsvFile(Some(lines.head), lines.tail)
      } else {
        CsvFile(None,lines)
      }
    }
  }
  
  def main(args:Array[String]) : Unit = {
    println("--")
    println(csvParser("test.csv", skipLines = 1))
    println("--")
  }
}