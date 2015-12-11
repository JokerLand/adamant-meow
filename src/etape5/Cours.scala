package etape5

import JaCoP.scala._

class Cours(intitule: String, aSuivre: Integer, bloc: Integer, paramCredits: Integer, quadriBloque : Integer) {

  val boolVarInscritsAuCours = new BoolVar(intitule)
  val intVarInscriptionAuCours = new IntVar(intitule, 0, 1)
  
  boolVarInscritsAuCours <=> (intVarInscriptionAuCours #= 1)
  
  val nom = intitule
  
  val disponible = aSuivre
  
  val blocCours = bloc
  
  val credits = paramCredits
  
  val blocant = quadriBloque

  override def toString() : String = intitule + " : reussi = " + aSuivre + " bloc = " + bloc + " credits = " + paramCredits
}