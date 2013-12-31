package refactoring.matlab.model

import   model._
import   model.statement._


object PragmaStatement {
  def apply(pragma: String) = new PragmaStatement(pragma)
  def unapply(s: PragmaStatement) = Some(s.pragma)
}

class PragmaStatement(var _pragma: String) extends Statement {
  update(pragma = _pragma)
  
  def pragma = _pragma
  
  def update(pragma: String = _pragma) = {
    _pragma = pragma
    this
  }
  
  // deep clone of the model.statement
  override def cloneStmt(): Statement = {
    val newStmt = PragmaStatement(pragma)
    newStmt.base_copyFrom(this)
    newStmt  
  }
  
  override def pretty(level: Int = 0): String = {
    indentStr(level) + pragma    
  }
  
  override def treePretty(level: Int = 0): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("PragmaStatement: ")
    str.append(pragma)
    str.append("\n")
    
    str.toString
  }  
}