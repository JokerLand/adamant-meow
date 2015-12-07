package etape1

import JaCoP.scala._

object Test extends jacop {

  def main(args: Array[String]): Unit = {
    val nbMax = 6;
    //val cours = List("ioo", "ianarch", "fsd", "asm", "math", "sd", "paoo", "c", "unix", "stat", "scala", "stage", "mobile", "internet");
    //val ECTS = List(2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 3, 1, 1)

    val ioo = (2, BoolVar("IOO"), 1); //ajouter 2 chiffres : un pour l année du cours et un pour savoir si le cours est reussi
    val iooInt = IntVar(ioo._2.id, 0, 1)
    ioo._2 <=> (iooInt #= 1)

    val algo = (3, BoolVar("Algo"), 1);
    val algoInt = IntVar(algo._2.id, 0, 1)
    algo._2 <=> (algoInt #= 1)

    val ianarch = (1, BoolVar("ianarch"), 0);
    val ianarchInt = IntVar(ianarch._2.id, 0, 1)
    ianarch._2 <=> (ianarchInt #= 1)

    val sd = (1, BoolVar("sd"), 1);
    val sdInt = IntVar(sd._2.id, 0, 1)
    sd._2 <=> (sdInt #= 1)

    if (ioo._3 == 0) ioo._2 #= 0
    if (algo._3 == 0) algo._2 #= 0
    if (ianarch._3 == 0) ianarch._2 #= 0
    if (sd._3 == 0) sd._2 #= 0

    ((ioo._1 * iooInt) + (algo._1 * algoInt) + (ianarch._1 * ianarchInt) + (sd._1 * sdInt)) #= 6

    val tousLesCours = List(ioo._2, algo._2, ianarch._2, sd._2);

    satisfy(search(tousLesCours, input_order, indomain_max));
    println(tousLesCours);

    /*    
    
    val cours = List(IntVar("ioo", 2, 2), IntVar("ianarch", 1, 1), IntVar("fsd", 1, 1), IntVar("asm", 1, 1), IntVar("math", 1, 1), IntVar("sd", 1, 1), IntVar("paoo", 1, 1), IntVar("c", 1, 1), IntVar("unix", 2, 2), IntVar("stat", 1, 1), IntVar("scala", 1, 1), IntVar("stage", 3, 3), IntVar("mobile", 1, 1), IntVar("internet", 1, 1))
    val UE = for (i <- List.range(0, 14)) yield BoolVar(cours(i).id)

    //val c = for (i <- List.range(0, 14) if UE(i).value == 1) yield cours(i)
    //sum(c) #= 6
    addAll(for (i <- List.range(0, 14) if UE(i).value == 1) yield cours(i).value()) #= 6
    
    def printSol(): Unit = {
      for (v <- UE) print(v.id + " " + v.value+ " ")
      println()
    }
    val result = satisfyAll(search(UE, input_order, indomain_min), printSol);
  }
 
  def addAll(c : List[Int]) : Int = {
    c match {
      case Nil => 0
      case y :: ys => addAll(ys) + y
    }*/
  }
} 