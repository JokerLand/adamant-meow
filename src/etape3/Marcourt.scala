package etape3

import JaCoP.scala._

object Marcourt extends jacop {
  
  def printSol(cours: List[IntVar]) {
    println("Voici la liste des cours que vous suivrez cette année : ")
    for (c <- cours if c.value == 1)
      println(c.id)
  }
  
  def main(args: Array[String]) {

    val ioo = (2, BoolVar("IOO"), 1);
    val iooInt = IntVar(ioo._2.id, 0, 1)
    ioo._2 <=> (iooInt #= 1)

    val algo = (3, BoolVar("Algo"), 1);
    val algoInt = IntVar(algo._2.id, 0, 1)
    algo._2 <=> (algoInt #= 1)

    val ianarch = (1, BoolVar("ianarch"), 1);
    val ianarchInt = IntVar(ianarch._2.id, 0, 1)
    ianarch._2 <=> (ianarchInt #= 1)

    val sd = (1, BoolVar("sd"), 1);
    val sdInt = IntVar(sd._2.id, 0, 1)
    sd._2 <=> (sdInt #= 1)

    val c = (3, BoolVar("c"), 1);
    val cInt = IntVar(c._2.id, 0, 1)
    c._2 <=> (cInt #= 1)

    val web = (2, BoolVar("web"), 1);
    val webInt = IntVar(web._2.id, 0, 1)
    web._2 <=> (webInt #= 1)

    val mobile = (2, BoolVar("mobile"), 1);
    val mobileInt = IntVar(mobile._2.id, 0, 1)
    mobile._2 <=> (mobileInt #= 1)

    val ia = (2, BoolVar("ia"), 1);
    val iaInt = IntVar(ia._2.id, 0, 1)
    ia._2 <=> (iaInt #= 1)

    val dae = (2, BoolVar("dae"), 1);
    val daeInt = IntVar(dae._2.id, 0, 1)
    dae._2 <=> (daeInt #= 1)

    val totalEctsReussis = ioo._1 * ioo._3 + algo._1 * algo._3 + ianarch._1 * ianarch._3 + sd._1 * sd._3 + c._1 * c._3 + web._1 * web._3 + mobile._1 * mobile._3 + ia._1 * ia._3 + dae._1 * dae._3
    val totalEctsRestant = 18 - totalEctsReussis

    val listeCoursIntVar = List(iooInt, algoInt, ianarchInt, sdInt, cInt, webInt, mobileInt, iaInt, daeInt)

    if (totalEctsReussis < 5) {
      ioo._2 #= ioo._3
      algo._2 #= algo._3
      ianarch._2 #= ianarch._3

      val listCoursBloc1 = List(iooInt, algoInt, ianarchInt)

      val b = satisfy(search(listCoursBloc1, input_order, indomain_max))
      if (b)
        printSol(listCoursBloc1)
      else
        println("Aucune solution trouvée")

    } else if (totalEctsRestant < 6) {
      ioo._2 #= ioo._3
      algo._2 #= algo._3
      ianarch._2 #= ianarch._3
      sd._2 #= sd._3
      c._2 #= c._3
      web._2 #= web._3
      mobile._2 #= mobile._3
      ia._2 #= ia._3
      dae._2 #= dae._3

      val b = satisfy(search(listeCoursIntVar, input_order, indomain_max))
      if (b)
        printSol(listeCoursIntVar)
      else
        println("Aucune solution trouvée")

    } else {

      if (ioo._3 == 0) ioo._2 #= 0
      if (algo._3 == 0) algo._2 #= 0
      if (ianarch._3 == 0) ianarch._2 #= 0
      if (sd._3 == 0) sd._2 #= 0
      if (c._3 == 0) c._2 #= 0
      if (web._3 == 0) web._2 #= 0
      if (mobile._3 == 0) mobile._2 #= 0
      if (ia._3 == 0) ia._2 #= 0
      if (dae._3 == 0) dae._2 #= 0

      //corequis : sd et ianarach sont corequis
      if (sd._3 == 1 && ianarch._3 == 1) sd._2 #= ianarch._2

      //prerequis : algo est un prerequis de sd et ianarch est un prerequis de web
      val prerequisalgo = IntVar("", 0, 1)
      if (algo._3 == 1) {
        sd._2 #= prerequisalgo
        sdInt #<= algoInt
      } else {
        prerequisalgo #= 0;
      }
      
      val prerequisianarch = IntVar("", 0, 1)
      if(ianarch._3 == 1) {
        web._2 #= prerequisianarch
        webInt #<= ianarchInt
      } else {
        prerequisianarch #= 0
      }
      
      val toutLesPrerequis = prerequisalgo + prerequisianarch
      
      //cours bloquants : ioo et ia bloquent le meme quadri
      NOT(AND((ioo._2 #= 1), (ia._2 #= 1)))

      //calcul nombre de credits
      ((ioo._1 * iooInt) + (algo._1 * algoInt) + (ianarch._1 * ianarchInt) + (sd._1 * sdInt) + (c._1 * cInt) + (web._1 * webInt) + (mobile._1 * mobileInt) + (ia._1 * iaInt) + (dae._1 * daeInt)) #= 6

      val b = minimize(search(listeCoursIntVar, input_order, indomain_max), toutLesPrerequis);
      if (b)
        printSol(listeCoursIntVar);
      else
        println("Pas de solution trouvée")
    }
  }
}