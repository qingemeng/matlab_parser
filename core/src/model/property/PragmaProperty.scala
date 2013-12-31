package model.property

// For storing LoopInfo(s) within statements 
class PragmaProperty() extends Property {

  // Domains ordered left to right = outer to inner
  private val _pragmas: scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map.empty

  def getPragmaParameter(keyword: String): Option[String] = {
    if(_pragmas.contains(keyword))
      Some(_pragmas(keyword))
    else
      None
  }
  
  def hasPragmaParameter(keyword: String): Boolean = {
    _pragmas.contains(keyword)
  }
  
  def appendPragmaMap(map: Map[String, String]){
    _pragmas ++= map
  }
  
  def setPragmaMap(map: Map[String, String]){
    _pragmas.clear
    _pragmas ++= map
  }
  
  def getPragmas(): Map[String, String] = {
    _pragmas.toMap
  }
  
  override def cloneProperty(): Property = {
    val newProp = new PragmaProperty()
    newProp._pragmas ++= this._pragmas
    newProp
  }
}