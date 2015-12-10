package finalStep

import JaCoP.scala._
import scala.io.Source

object Marcourt extends jacop {

  def main(args: Array[String]): Unit = {

    def creerCours(fichierCours: String, fichierEleve: String): List[Cours] = {
      def creerCour(itCours: Iterator[String], itEleve: Iterator[String]): List[Cours] = {
        if (itCours.hasNext && itEleve.hasNext) {
          val stringCour = itCours.next.split(";")
          val stringEleve = itEleve.next.split(";")

          new Cours(stringCour(0), stringEleve(1).toInt, stringCour(2).toInt, stringCour(1).toInt, stringCour(3).toInt) :: creerCour(itCours, itEleve)
        } else
          Nil
      }

      val itCours = Source.fromFile(fichierCours).getLines.filterNot(_.startsWith("#"))
      val itEleve = Source.fromFile(fichierEleve).getLines.filterNot(_.startsWith("#"))

      creerCour(itCours, itEleve)
    }

    def creerCorequis(toutCorequis: List[List[Cours]]) {
      for (listeCorequis <- toutCorequis) {
        for (i <- 0 until listeCorequis.length - 1) {
          if (listeCorequis(i).disponible == 1 && listeCorequis(i + 1).disponible == 1)
            listeCorequis(i).booleen #= listeCorequis(i + 1).booleen
        }
      }
    }

    def creerPrerequis(toutPrerequis: List[List[Cours]]): List[IntVar] = {
      val listConversion = for (prerequis <- toutPrerequis) yield IntVar("", 0, 1)

      for (i <- 0 until toutPrerequis.length) {
        if ((toutPrerequis(i))(1).disponible == 1) {
          (toutPrerequis(i))(0).booleen #= listConversion(i)
          (toutPrerequis(i))(0).ok #<= (toutPrerequis(i))(1).ok
        } else {
          listConversion(i) #= 0
        }
      }
      listConversion
    }

    def append(x: List[IntVar], y: List[IntVar]): List[IntVar] = {
      x match {
        case Nil => y match {
          case Nil => Nil
          case ye :: ys => ye :: append(x, ys)
        }
        case xe :: xs => xe :: append(xs, y)
      }
    }

    def creerCoursBloquant(cours: List[Cours]): List[IntVar] = {
      val liste1 = for (c <- cours if c.blocant == 1) yield c
      val liste2 = for (c <- cours if c.blocant == 2) yield c

      append(creerContraintesCoursBloquants(liste1), creerContraintesCoursBloquants(liste2))
    }

    def creerContraintesCoursBloquants(cours: List[Cours]): List[IntVar] = {

      cours match {
        case Nil => Nil
        case tete :: queue => {
          val intvars = for (i <- List.range(1, cours.length)) yield new IntVar("", 0, 1)
          for (i <- 1 until cours.length) {
            (tete.booleen /\ cours(i).booleen) <=> (intvars(i - 1) #= 1)
          }
          append(creerContraintesCoursBloquants(queue), intvars)
        }
      }
    }

    def lireCoPreRequis(fichier: String, cours: List[Cours]): List[List[Cours]] = {
      val t = Source.fromFile(fichier).getLines.filterNot(_.startsWith("#"))
      val list = (for (a <- t) yield List(a.split(";")(0).toInt - 1, a.split(";")(1).toInt - 1)).toList
      for (a <- list) yield List(cours(a(0)), cours(a(1)))
    }

    def addEcts(x: Int, y: Cours): Int = x + y.ects

    def sommeCoursEctsRestants(cours: List[Cours]): Int = {
      cours match {
        case Nil => 0
        case tete :: queue => tete.ects * tete.disponible + sommeCoursEctsRestants(queue)
      }
    }

    def addIntVar(x: IntVar, y: IntVar): IntVar = x + y

    def sommeCours(cours: List[Cours]): IntVar = {
      cours match {
        case Nil => IntVar("", 0, 0)
        case tete :: queue => (tete.ok * tete.ects) + sommeCours(queue)
      }
    }

    def printSol(cours: List[IntVar]) {
      println("Voici la liste des cours que vous suivrez cette année : ")
      for (c <- cours if c.value == 1)
        println(c.id)
    }

    //creation des objets Cours et les mettre dans une liste
    val cours = creerCours("cours.txt", "eleve.txt")
    println(cours)
    val coursBloc1 = for (a <- cours if a.blocCours == 1) yield a

    val prerequis = lireCoPreRequis("prerequis.txt", cours)
    val corequis = lireCoPreRequis("corequis.txt", cours)

    //creation des conditions pour les prerequis et corequis
    creerCorequis(corequis)
    val listConversionPrerequis = creerPrerequis(prerequis)
    val listConversionCoursBloquants = creerCoursBloquant(cours)

    //liste contenant les IntVar des cours
    val coursIntVar = for (c <- cours) yield c.ok

    val nbCredits = cours.foldLeft(0)(addEcts)
    val creditAnnee = nbCredits / 3 // = 60
    val creditsMinimumAReussirBloc1 = creditAnnee / 4 * 3 // = 45

    val prerequisalgo = IntVar("", 0, 1)

    //Calcul des credits deja reussis

    val totalCreditsRestant = sommeCoursEctsRestants(cours)
    val totalEctsReussis = nbCredits - totalCreditsRestant

    if (totalEctsReussis < creditsMinimumAReussirBloc1) {
      println("PREMIERE (" + totalEctsReussis + " ects reussis)")
      val coursBloc1 = for (c <- cours if c.blocCours == 1) yield c.ok

      for (c <- cours if c.blocCours == 1)
        c.booleen #= c.disponible

      val b = satisfy(search(coursBloc1, input_order, indomain_max))
      if (b)
        printSol(coursBloc1)
      else
        println("Aucune solution trouvée")

    } else if (totalCreditsRestant < 60) {
      println("total ects restants = " + totalCreditsRestant)
      for (c <- cours) {
        c.booleen #= c.disponible
      }
      val b = satisfy(search(coursIntVar, input_order, indomain_max))
      if (b)
        printSol(coursIntVar)
      else
        println("Aucune solution trouvée")

    } else {

      for (c <- cours if c.disponible == 0)
        c.booleen #= 0

      val coursBlocants = listConversionCoursBloquants.foldLeft(IntVar("", 0, 0))(addIntVar)
      val nombredeconversion = listConversionPrerequis.foldLeft(IntVar("", 0, 0))(addIntVar)

      val transgression = nombredeconversion + coursBlocants * (listConversionPrerequis.length + 1)

      sommeCours(cours) #= creditAnnee

      val b = minimize(search(coursIntVar, input_order, indomain_max), transgression)
      if (b) {
        println("nombre de conversion prerequis/corequis = " + nombredeconversion.value)
        println("nombre de cours bloquant le meme quadrimestre = " + coursBlocants.value)
        printSol(coursIntVar)
      } else
        println("Aucune solution trouvée")

    }
  }
} 