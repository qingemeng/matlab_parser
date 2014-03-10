package model.statement

import model.property._
import model.BasicType

abstract class Statement extends HasProperties with StencilProperty {
  var cloneSrc: Option[Statement] = None
  private var _parent: Option[Statement] = None
  var _typeInfo: Map[String, BasicType] = Map.empty
  
  // pretty print
  protected def indentStr(level: Int): String = "  " * level
  def pretty(level: Int): String = indentStr(level) + pretty()
  def pretty(): String = ???
  def treePretty(level: Int = 0): String = ???
//  def typePretty(level: Int = 0): String = ???
  
  // deep clone of the model.statement
  def cloneStmt(): Statement = ???
  
  // copy attributes from given model.statement to this model.statement
   def base_copyFrom(stmt: Statement) {
    //Only keep track of the previous clone
    stmt.cloneSrc = None
    cloneSrc = Some(stmt)
    copyProperties(stmt)
  }
  
  def parent = _parent
  def getParent = _parent.getOrElse(null)
  def setParent(p: Statement) = if (p == null) _parent = None else _parent = Some(p)
  def typeInfo: Map[String, BasicType] = _typeInfo

}

