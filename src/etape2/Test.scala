package etape2

import JaCoP.scala._

object Test extends jacop {

  def main(args: Array[String]): Unit = {
    val nbMax = 6;

    //transgression prerequis
    val trans = IntVar("trans", 0, 1);

    //nbCred, cours inscrit dans le prog, cours disponible
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

    val c = (2, BoolVar("c"), 1);
    val cInt = IntVar(c._2.id, 0, 1)
    c._2 <=> (cInt #= 1)

    val web = (1, BoolVar("web"), 1);
    val webInt = IntVar(web._2.id, 0, 1)
    web._2 <=> (webInt #= 1)

    if (ioo._3 == 0) ioo._2 #= 0
    if (algo._3 == 0) algo._2 #= 0
    if (ianarch._3 == 0) ianarch._2 #= 0
    if (sd._3 == 0) sd._2 #= 0
    if (c._3 == 0) c._2 #= 0
    if (web._3 == 0) web._2 #= 0

    //    if(trans.value == 0) {
    //    	if(algo._3 == 1)
    //    	  ioo._2 #= 0 //prerequis, si tu dois encore passer algo, tu peux pas avoir le cours de ioo
    //    } else {
    //      if(ioo._3 == 1 && algo._3 == 1) {
    //        ioo._2 #= algo._2
    //      }
    //    }

    //prerequis													//transgression -> corequis
    OR(if (algo._3 == 1) { ioo._2 #= 0; trans #= 0 } else 0 #= 0, if (ioo._3 == 1 && algo._3 == 1) { ioo._2 #= algo._2; trans #= 1 } else 0 #= 0) // OR = xor?
    //NOT(AND(if(algo._3 == 1) {ioo._2 #= 0; trans #= 0} else 0 #= 0, if(ioo._3 == 1 && algo._3 == 1) { ioo._2 #= algo._2; trans #= 1} else 0 #= 0))

    if (sd._3 == 1 && ianarch._3 == 1) sd._2 #= ianarch._2 //corequis

    ((ioo._1 * iooInt) + (algo._1 * algoInt) + (ianarch._1 * ianarchInt) + (sd._1 * sdInt) + (c._1 * cInt) + (web._1 * webInt)) #= 6

    val tousLesCours = List(ioo._2, algo._2, ianarch._2, sd._2, c._2, web._2);

    satisfy(search(tousLesCours, input_order, indomain_max));
    println(tousLesCours);
    statistics
  }
} 