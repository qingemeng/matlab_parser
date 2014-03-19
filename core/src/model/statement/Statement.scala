package model.statement

import model.property._
import model.BasicType
import scala.util.parsing.input.Positional

abstract class Statement extends HasProperties with Positional with StencilProperty {
  var cloneSrc: Option[Statement] = None
  private var _parent: Option[Statement] = None
//  private var _position = this.pos
  var _typeInfo: Map[String, BasicType] = Map.empty
  
  // pretty print
  protected def indentStr(level: Int): String = "  " * level
  def pretty(level: Int): String = indentStr(level) + pretty()
  def pretty(): String = ???
  def treePretty(level: Int = 0): String = ???
  def semanticAnalyse(level: Int = 0): String = ???

//  def getLineNumber()
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
//  def position = _position
//  def getPosition = _position
//  def setPosition (pos:Positional) = if (pos == null) _position = null else _position = this.pos

}

