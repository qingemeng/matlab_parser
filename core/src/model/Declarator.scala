package   model

import   model._
import   model.statement._
import   model.expression._

object Declarator {
  def apply(idName: IdName, initializer: Option[Expr]) = new Declarator(idName, initializer)
  def apply(idName: IdName, initializer: Expr) = new Declarator(idName, Some(initializer))
  def apply(idName: IdName) = new Declarator(idName, None)
  def unapply(d: Declarator) = Some(d.idName, d.initializer)
}

class Declarator(val idName: IdName, private var _initializer: Option[Expr]) {

  def initializer = _initializer
  
  def update(initializer: Option[Expr] = _initializer) = {
    _initializer = initializer
    this
  }
  
  def cloneDecl(): Declarator = Declarator(idName.cloneName, initializer match {
    case Some(expr) => Some(expr.cloneExpr)
    case None		=> None
  })
  
  def pretty(hash: Boolean = false): String = {
    if (initializer.isDefined)
      idName.pretty(hash) + " = " + initializer.get.pretty(hash)
    else
      idName.pretty(hash)
  }
  
  def treePretty(level: Int = 0, hash: Boolean = false): String = {
    def indentStr(level: Int): String = "  " * level
    
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("Declarator: ")
    str.append(pretty(hash))
    str.append("\n")
    
    str.append(indentStr(level))
    str.append("->Name:\n")
    str.append(idName.treePretty(level+2, false))
    
    if (initializer.isDefined){
      str.append(indentStr(level))
      str.append("->Initializer:\n")
      str.append(initializer.get.treePretty(level+2))
    }
    
    str.toString
  }
}