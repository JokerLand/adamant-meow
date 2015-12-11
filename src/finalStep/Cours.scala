package finalStep

import JaCoP.scala._

class Cours(intitule: String, aSuivre: Integer, bloc: Integer, credits: Integer, quadriBloque : Integer) {

  val booleen = new BoolVar(intitule)
  val intVarInscriptionAuCours = new IntVar(intitule, 0, 1)
  
  booleen <=> (intVarInscriptionAuCours #= 1)
  
  val nom = intitule
  
  val disponible = aSuivre
  
  val blocCours = bloc
  
  val ects = credits
  
  val blocant = quadriBloque

  override def toString() : String = intitule + " : reussi = " + aSuivre + " bloc = " + bloc + " credits = " + credits
}