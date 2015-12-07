package etape3

import JaCoP.scala._

class Cours(intitule: String, dejaReussi: Integer, bloc: Integer, credits: Integer) {

  val booleen = new BoolVar(intitule);
  val ok = new IntVar(intitule, 0, 1)
  
  booleen <=> (ok #= 1)

  def getBool() : BoolVar = booleen
  
  def getInt() : IntVar = ok;
  
  def getIntitule() : String = intitule;
  
  def getReussi() : Integer = dejaReussi;
  
  def getBloc() : Integer = bloc;
  
  def getCredits() : Integer = credits;

  override def toString() : String = intitule + " : reussi = " + dejaReussi + " bloc = " + bloc + " credits = " + credits
}