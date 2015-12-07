package etape3

import JaCoP.scala._

class Cours(intitule: String, dejaReussi: Integer, bloc: Integer, credits: Integer, quadriBloque : Integer) {

  val booleen = new BoolVar(intitule)
  val ok = new IntVar(intitule, 0, 1)
  
  booleen <=> (ok #= 1)
  
  val nom = intitule
  
  val reussi = dejaReussi
  
  val blocCours = bloc
  
  val ects = credits
  
  val blocant = quadriBloque

  override def toString() : String = intitule + " : reussi = " + dejaReussi + " bloc = " + bloc + " credits = " + credits
}