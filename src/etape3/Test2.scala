package etape3

import JaCoP.scala._

object Test2 extends jacop {

  def main(args: Array[String]): Unit = {
    def creerCours(noms: List[String], ects: List[Int], bloc: List[Int], dejaReussi: List[Int]): IndexedSeq[Cours] = {
      def creerCour(intitule: String, dejaReussi: Integer, bloc: Integer, credits: Integer): Cours = {
        new Cours(intitule, dejaReussi, bloc, credits)
      }

      val cours = for (a <- 0 until noms.length) yield creerCour(noms(a), dejaReussi(a), bloc(a), ects(a))
      cours
    }
    
    val noms = List("a", "b")
    val ects = List(1, 2)
    val bloc = List(1, 2)
    val dejaReussi = List(0, 1)
    
    val cours = creerCours(noms, ects, bloc, dejaReussi)

    for(a <- 0 until cours.length)
    	println(cours(a));
  }
}