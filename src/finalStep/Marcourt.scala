package finalStep

import JaCoP.scala._

object Marcourt extends jacop {

  def main(args: Array[String]): Unit = {

    def creerCours(noms: List[String], ects: List[Int], bloc: List[Int], dejaReussi: List[Int], quadriBloque: List[Int]): List[Cours] = {
      def creerCour(intitule: String, dejaReussi: Integer, bloc: Integer, credits: Integer, quadriBloque: Integer): Cours = {
        new Cours(intitule, dejaReussi, bloc, credits, quadriBloque)
      }

      val cours = for (a <- List.range(0, noms.length)) yield creerCour(noms(a), dejaReussi(a), bloc(a), ects(a), quadriBloque(a))
      cours
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

    //interface insertion cours
    val intitules = List("APOO", "coursb", "coursc", "coursd", "course", "coursf")
    val ects = List(5, 6, 9, 8, 5, 3)
    val bloc = List(1, 1, 1, 1, 2, 2)
    val quadriBloque = List(0, 0, 1, 1, 2, 2)

    //interface eleve
    val reussi = List(1, 1, 0, 1, 1, 0)

    //creation des objets Cours et les mettre dans une liste
    val cours = creerCours(intitules, ects, bloc, reussi, quadriBloque)
    val coursBloc1 = for (a <- cours if a.blocCours == 1) yield a

    //interface cours prerequis / corequis
    //utilisez cours(a) ou a designe l index du courss
    //1er liste a comme prerequis le 2eme
    val prerequis = List(List(cours(1), cours(2)), List(cours(1), cours(3)), List(cours(2), cours(5)))
    val corequis = List(List(cours(1), cours(2), cours(3)), List(cours(4), cours(5)))

//        val apoo = (5, BoolVar("APOO"), 1, 1)
//    val apooInt = IntVar(apoo._2.id, 0, 1)
//    apoo._2 <=> (apooInt #= 1)
//
//    val algo = (6, BoolVar("Algo"), 1, 1)
//    val algoInt = IntVar(algo._2.id, 0, 1)
//    algo._2 <=> (algoInt #= 1)
//
//    val ianarch = (5, BoolVar("ianarch"), 1, 1)
//    val ianarchInt = IntVar(ianarch._2.id, 0, 1)
//    ianarch._2 <=> (ianarchInt #= 1)
//
//    val prograWeb = (4, BoolVar("progra Web"), 1, 1)
//    val prograWebInt = IntVar(prograWeb._2.id, 0, 1)
//    prograWeb._2 <=> (prograWebInt #= 1)
//
//    val sd = (6, BoolVar("sd"), 1, 1)
//    val sdInt = IntVar(sd._2.id, 0, 1)
//    sd._2 <=> (sdInt #= 1)
//
//    val ios = (5, BoolVar("intro à l os"), 1, 1)
//    val iosInt = IntVar(ios._2.id, 0, 1)
//    ios._2 <=> (iosInt #= 1)
//
//    val fo = (6, BoolVar("Fonctionnement ordinateur"), 1, 1)
//    val foInt = IntVar(fo._2.id, 0, 1)
//    fo._2 <=> (foInt #= 1)
//
//    val eco = (5, BoolVar("eco + compta"), 1, 1)
//    val ecoInt = IntVar(eco._2.id, 0, 1)
//    eco._2 <=> (ecoInt #= 1)
//
//    val math1 = (4, BoolVar("math1"), 1, 1)
//    val math1Int = IntVar(math1._2.id, 0, 1)
//    math1._2 <=> (math1Int #= 1)
//
//    val math2 = (6, BoolVar("math2"), 1, 1)
//    val math2Int = IntVar(math2._2.id, 0, 1)
//    math2._2 <=> (math2Int #= 1)
//
//    val pdw = (4, BoolVar("projet developpement web"), 1, 1)
//    val pdwInt = IntVar(pdw._2.id, 0, 1)
//    pdw._2 <=> (pdwInt #= 1)
//
//    val ang1 = (4, BoolVar("Anglais 1"), 1, 1)
//    val ang1Int = IntVar(ang1._2.id, 0, 1)
//    ang1._2 <=> (ang1Int #= 1)
//
//    //BLOC 2
//    val c = (2, BoolVar("c"), 1, 2)
//    val cInt = IntVar(c._2.id, 0, 1)
//    c._2 <=> (cInt #= 1)
//
//    val paj = (4, BoolVar("Programmation Avancée en Java"), 1, 2)
//    val pajInt = IntVar(paj._2.id, 0, 1)
//    paj._2 <=> (pajInt #= 1)
//
//    val con = (6, BoolVar("Analyse et Conception"), 1, 2)
//    val conInt = IntVar(con._2.id, 0, 1)
//    con._2 <=> (conInt #= 1)
//
//    val gestDon = (6, BoolVar("Gestion des Données"), 1, 2)
//    val gestDonInt = IntVar(gestDon._2.id, 0, 1)
//    gestDon._2 <=> (gestDonInt #= 1)
//
//    val prograWebAvc = (6, BoolVar("Programmation Web avancée"), 1, 2)
//    val prograWebAvcInt = IntVar(prograWebAvc._2.id, 0, 1)
//    prograWebAvc._2 <=> (prograWebAvcInt #= 1)
//
//    val sipp = (4, BoolVar("Systeme informatique : principes et protocoles"), 1, 2)
//    val sippInt = IntVar(sipp._2.id, 0, 1)
//    sipp._2 <=> (sippInt #= 1)
//
//    val telecom = (3, BoolVar("Internet : principes et protocoles avances"), 1, 2)
//    val telecomInt = IntVar(telecom._2.id, 0, 1)
//    telecom._2 <=> (telecomInt #= 1)
//
//    val orga = (6, BoolVar("Organisation des entreprises"), 1, 2)
//    val orgaInt = IntVar(orga._2.id, 0, 1)
//    orga._2 <=> (orgaInt #= 1)
//
//    val design = (6, BoolVar("Designe d'application entreprise"), 1, 2)
//    val designInt = IntVar(design._2.id, 0, 1)
//    design._2 <=> (designInt #= 1)
//
//    val unix = (5, BoolVar("Unix"), 1, 2)
//    val unixInt = IntVar(unix._2.id, 0, 1)
//    unix._2 <=> (unixInt #= 1)
//
//    val mobile = (4, BoolVar("Informatique mobile"), 1, 2)
//    val mobileInt = IntVar(mobile._2.id, 0, 1)
//    mobile._2 <=> (mobileInt #= 1)
//
//    val ang2 = (5, BoolVar("Anglais 2"), 1, 2)
//    val ang2Int = IntVar(ang2._2.id, 0, 1)
//    ang2._2 <=> (ang2Int #= 1)
//
//    val pjqs = (5, BoolVar("Programmation Java : Questions Speciale"), 1, 2)
//    val pjqsInt = IntVar(pjqs._2.id, 0, 1)
//    pjqs._2 <=> (pjqsInt #= 1)
//
//    val adminSys = (4, BoolVar("Administration et Securité des Systemes"), 1, 2)
//    val adminSysInt = IntVar(adminSys._2.id, 0, 1)
//    adminSys._2 <=> (adminSysInt #= 1)
//
//    val adminRes = (5, BoolVar("Administration et Secrité des Reseaux"), 1, 2)
//    val adminResInt = IntVar(adminRes._2.id, 0, 1)
//    adminRes._2 <=> (adminResInt #= 1)
//
//    val ang3 = (4, BoolVar("Anlgais 3"), 1, 2)
//    val ang3Int = IntVar(ang3._2.id, 0, 1)
//    ang3._2 <=> (ang3Int #= 1)
//
//    val msp = (3, BoolVar("Mise en situation professionelle"), 1, 2)
//    val mspInt = IntVar(msp._2.id, 0, 1)
//    msp._2 <=> (mspInt #= 1)
//
//    val stage = (30, BoolVar("Activité d'intégration en milieu professionel"), 1, 2)
//    val stageInt = IntVar(stage._2.id, 0, 1)
//    stage._2 <=> (stageInt #= 1)
//
//    val ia = (4, BoolVar("Intelligence artificielle"), 1, 2)
//    val iaInt = IntVar(ia._2.id, 0, 1)
//    ia._2 <=> (iaInt #= 1)
//
//    val rempl = (8, BoolVar("Remlissage"), 1, 2)
//    val remplInt = IntVar(rempl._2.id, 0, 1)
//    rempl._2 <=> (remplInt #= 1)

    //creation des conditions pour les prerequis et corequis
    creerCorequis(corequis)
    val listConversionPrerequis = creerPrerequis(prerequis)
    val listConversionCoursBloquants = creerCoursBloquant(cours)

    val coursIntVar = for (c <- cours) yield c.ok

    def addInt(x : Int, y :Int) : Int = x + y
    val nbCredits = ects.foldLeft(0)(addInt)
    val creditAnnee = nbCredits / 3 // = 60
    val creditsBloc1 = creditAnnee / 4 * 3 // = 45

    val prerequisalgo = IntVar("", 0, 1)

    //nbCred, cours inscrit dans le prog, cours disponible, bloc du cours

    //Calcul des credits deja reussis

    def sommeCoursEctsRestants(cours: List[Cours]): Int = {
      cours match {
        case Nil => 0
        case tete :: queue => tete.ects * tete.disponible + sommeCoursEctsRestants(queue)
      }
    }
    val totalEctsRestant = sommeCoursEctsRestants(cours)
    val totalEctsReussis = nbCredits - totalEctsRestant
    println("total ects reussis = " + totalEctsReussis)
    
    if (totalEctsReussis < creditsBloc1) {
      println("PREMIERE (" + totalEctsReussis + " ects reussis)")
      val coursBloc1 = for(c <- cours if c.blocCours == 1) yield c.ok

      for (c <- cours if c.blocCours == 1)
        c.booleen #= c.disponible

      println(satisfy(search(coursBloc1, input_order, indomain_max)))
      println(coursBloc1)

    } else if (totalEctsRestant < 60) {
      println("total ects restants = " + totalEctsRestant)
      for (c <- cours) {
        c.booleen #= c.disponible
      }
      println(satisfy(search(coursIntVar, input_order, indomain_max)))
      println(coursIntVar)

    } else {

      for (c <- cours if c.disponible == 0)
        c.booleen #= 0

      def addIntVar(x: IntVar, y: IntVar): IntVar = {
        x + y
      }
      val coursBlocants = listConversionCoursBloquants.foldLeft(IntVar("", 0, 0))(addIntVar)
      val nombredeconversion = listConversionPrerequis.foldLeft(IntVar("", 0, 0))(addIntVar)

      val transgression = nombredeconversion + coursBlocants * (listConversionPrerequis.length +1)

      def sommeCours(cours: List[Cours]): IntVar = {
        cours match {
          case Nil => IntVar("", 0, 0)
          case tete :: queue => (tete.ok * tete.ects) + sommeCours(queue)
        }
      }

      sommeCours(cours) #= creditAnnee

      println(minimize(search(coursIntVar, input_order, indomain_max), transgression))
      println("nbConv " + nombredeconversion)
      println("nbCoursBlocants " + coursBlocants)
      println(coursIntVar)
    }
  }
} 