package refactoring.matlab.model

import   model._
import   model.statement._

object FunctionDefStatement {
  def apply(funcDef: FunctionDef) = new FunctionDefStatement(funcDef)
  def unapply(s: FunctionDefStatement) = Some(s.funcDef)
}

class FunctionDefStatement(var _funcDef: FunctionDef) extends Statement{
  update(funcDef = _funcDef)
  
  def funcDef = _funcDef
  
  def update(funcDef: FunctionDef = _funcDef) = {
    _funcDef = funcDef
    this
  }
  
  // deep clone of the model.statement
  override def cloneStmt(): Statement = {
    val newStmt = FunctionDefStatement(funcDef.cloneFunc)
    newStmt.base_copyFrom(this)
    newStmt  
  }
  
  override def pretty(level: Int = 0): String = {
    indentStr(level) + funcDef.pretty()    
  }
  
  override def treePretty(level: Int = 0): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("FunctionDefStatement: ")
    //str.append(pretty())
    str.append("\n")
    
    str.append(funcDef.treePretty(level+1))
    
    str.toString
  }
}