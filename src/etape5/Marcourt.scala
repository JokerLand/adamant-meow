package etape5

import JaCoP.scala._
import scala.io.Source

object Marcourt extends jacop {

  //l utilite des differents fichiers .txt est expliquée au debut de ceux ci
  val COURS = "cours.txt"
  val ELEVE = "eleve.txt"
  val PREREQUIS = "prerequis.txt"
  val COREQUIS = "corequis.txt"

  def main(args: Array[String]): Unit = {

    /**
     * construit la liste des cours sur base de deux fichiers de configuration
     * @param fichierCours le nom du fichier de configuration des cours
     * @param fichierEleve le nom du fichier de configuration de l eleve
     * @return la liste de cours
     */
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

    /**
     * Pose Les conditions nécessaires sur les corequis
     * @param toutCorequis est la liste de tout les corequis
     */
    def creerCorequis(toutCorequis: List[List[Cours]]) {
      for (listeCorequis <- toutCorequis) {
        for (i <- 0 until listeCorequis.length - 1) {
          if (listeCorequis(i).disponible == 1 && listeCorequis(i + 1).disponible == 1)
            listeCorequis(i).boolVarInscritsAuCours #= listeCorequis(i + 1).boolVarInscritsAuCours
        }
      }
    }

    /**
     * Pose les conditions nécessaires sur les corequis
     * @param toutPrerequis la liste de tout les prerequis
     * @return une liste d IntVar permettant de savoir si un prérequis a été converti en corequis
     */
    def creerPrerequis(toutPrerequis: List[List[Cours]]): List[IntVar] = {
      val listConversionDePrerequisACorequis = for (prerequis <- toutPrerequis) yield IntVar("", 0, 1)

      for (i <- 0 until toutPrerequis.length) {
        if ((toutPrerequis(i))(1).disponible == 1) {
          (toutPrerequis(i))(0).boolVarInscritsAuCours #= listConversionDePrerequisACorequis(i)
          (toutPrerequis(i))(0).intVarInscriptionAuCours #<= (toutPrerequis(i))(1).intVarInscriptionAuCours
        } else {
          listConversionDePrerequisACorequis(i) #= 0
        }
      }
      listConversionDePrerequisACorequis
    }

    /**
     * Cree une liste d'IntVar en en collant deux
     * @param x une liste
     * @param y une liste
     * @return la liste créée
     */
    def append(x: List[IntVar], y: List[IntVar]): List[IntVar] = {
      x match {
        case Nil => y match {
          case Nil => Nil
          case ye :: ys => ye :: append(x, ys)
        }
        case xe :: xs => xe :: append(xs, y)
      }
    }

    /**
     * Pose les conditions nécessaires pour les cours blocants
     * @param cours la liste de tout les cours
     * @return une liste d IntVar permettants de savoir si une contrainte de cours bloquant a été transgressée
     */
    def creerCoursBloquant(cours: List[Cours]): List[IntVar] = {
      def creerContraintesCoursBloquants(cours: List[Cours]): List[IntVar] = {

        cours match {
          case Nil => Nil
          case tete :: queue => {
            val intvars = for (i <- List.range(1, cours.length)) yield new IntVar("", 0, 1)
            for (i <- 1 until cours.length) {
              (tete.boolVarInscritsAuCours /\ cours(i).boolVarInscritsAuCours) <=> (intvars(i - 1) #= 1)
            }
            append(creerContraintesCoursBloquants(queue), intvars)
          }
        }
      }

      val liste1 = for (c <- cours if c.blocant == 1) yield c
      val liste2 = for (c <- cours if c.blocant == 2) yield c

      append(creerContraintesCoursBloquants(liste1), creerContraintesCoursBloquants(liste2))
    }

    /**
     * Crée les listes formatées correctement pour les corequis ou prerequis
     * @param fichier le nom du fichier a traiter
     * @param cours la liste entiere des cours
     * @return une liste de corequis ou prerequis formatée correctement
     */
    def lireCoPreRequis(fichier: String, cours: List[Cours]): List[List[Cours]] = {
      val t = Source.fromFile(fichier).getLines.filterNot(_.startsWith("#"))
      val list = (for (a <- t) yield List(a.split(";")(0).toInt - 1, a.split(";")(1).toInt - 1)).toList
      for (a <- list) yield List(cours(a(0)), cours(a(1)))
    }

    /**
     * Additionne un int et les credits d un cours
     * @param x un int
     * @param y un cours
     * @return la somme de l int et des crédits du cours
     */
    def addCredits(x: Int, y: Cours): Int = x + y.credits

    def sommeCoursCreditsRestants(cours: List[Cours]): Int = {
      cours match {
        case Nil => 0
        case tete :: queue => tete.credits * tete.disponible + sommeCoursCreditsRestants(queue)
      }
    }

    /**
     * Additionne deux IntVar
     * @param x un IntVar
     * @param y un IntVar
     * @return la somme des deux IntVar
     */
    def addIntVar(x: IntVar, y: IntVar): IntVar = x + y

    /**
     * Additionne le nombre de crédits des cours qui seront dans le programme de l eleve
     * @param la liste de tout les cours
     * @return un IntVar
     */
    def sommeCours(cours: List[Cours]): IntVar = {
      cours match {
        case Nil => IntVar("", 0, 0)
        case tete :: queue => (tete.intVarInscriptionAuCours * tete.credits) + sommeCours(queue)
      }
    }

    /**
     * Affichage des cours qui sont dans le programme de l eleve
     * @param la liste des IntVar des cours
     */
    def printSol(cours: List[IntVar]) {
      println("Voici la liste des cours que vous suivrez cette année : ")
      for (c <- cours if c.value == 1)
        println(c.id)
    }

    //creation des objets Cours et les mettre dans une liste
    val cours = creerCours(COURS, ELEVE)

    //liste contenant les IntVar des cours
    val coursIntVar = for (c <- cours) yield c.intVarInscriptionAuCours

    //calcul du nombre de credits, credits par année et credits necessaire pour passer au bloc 2
    val nbCredits = cours.foldLeft(0)(addCredits)
    if (nbCredits != 180)
      println("Le nombre de credits est differents de 180 (" + nbCredits + "), cela peut poser des problemes au programme")
    val creditParAnnee = nbCredits / 3 // = 60
    val creditsMinimumAReussirBloc1 = creditParAnnee / 4 * 3 // = 45

    //Calcul des credits deja reussis et restants
    val totalCreditsRestant = sommeCoursCreditsRestants(cours)
    val totalCreditsReussis = nbCredits - totalCreditsRestant

    if (totalCreditsReussis < creditsMinimumAReussirBloc1) { //si l eleve a reussi moins de 45 credits, il reste dans le bloc 1
      val coursBloc1 = for (c <- cours if c.blocCours == 1) yield c.intVarInscriptionAuCours

      for (c <- cours if c.blocCours == 1) //l eleve ne doit passer que les cours disponibles dont il dispose dans le bloc 1
        c.boolVarInscritsAuCours #= c.disponible

      val b = satisfy(search(coursBloc1, input_order, indomain_max))
      if (b)
        printSol(coursBloc1)
      else
        println("Aucune solution trouvée")

    } else if (totalCreditsRestant < 60) { //si il reste moins de 60 credits a passer a l eleve, il passe tout les cours qu il n a pas encore reussi
      
      for (c <- cours) //l eleve ne doit passer que les cours disponibles dont il dispose
        c.boolVarInscritsAuCours #= c.disponible

      val b = satisfy(search(coursIntVar, input_order, indomain_max))
      if (b)
        printSol(coursIntVar)
      else
        println("Aucune solution trouvée")

    } else { //cas général

      //creation des conditions pour les prerequis et corequis
      val prerequis = lireCoPreRequis(PREREQUIS, cours)
      val corequis = lireCoPreRequis(COREQUIS, cours)
      creerCorequis(corequis)
      val listConversionPrerequis = creerPrerequis(prerequis)
      val listConversionCoursBloquants = creerCoursBloquant(cours)

      for (c <- cours if c.disponible == 0) //si l eleve a deja reussi un cours, il ne doit plus le repasser
        c.boolVarInscritsAuCours #= 0

      val coursBlocants = listConversionCoursBloquants.foldLeft(IntVar("", 0, 0))(addIntVar)
      val nombredeconversion = listConversionPrerequis.foldLeft(IntVar("", 0, 0))(addIntVar)

      val transgressionTotal = nombredeconversion + coursBlocants * (listConversionPrerequis.length + 1)

      sommeCours(cours) #= creditParAnnee

      val b = minimize(search(coursIntVar, input_order, indomain_max), transgressionTotal)
      if (b) {
        println("nombre de conversion prerequis/corequis = " + nombredeconversion.value)
        println("nombre de cours bloquant le meme quadrimestre = " + coursBlocants.value)
        printSol(coursIntVar)
      } else
        println("Aucune solution trouvée")

    }
  }
} 