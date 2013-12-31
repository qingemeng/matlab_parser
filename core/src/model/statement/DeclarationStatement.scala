package model.statement

import   model._
import   model.statement._
import   model.expression._

object DeclarationStatement {
  def apply(decls: Declarator) = new DeclarationStatement(List(decls))
  def apply(decls: List[Declarator]) = {
    new DeclarationStatement(decls)
  }
  def unapply(s: DeclarationStatement) = Some(s.decls)
}

class DeclarationStatement(private var _decls: List[Declarator]) extends Statement {
  
  def decls = _decls
  
  def update(decls: List[Declarator] = _decls) = {
    _decls = decls
    this
  }
  
  // deep clone of the model.statement
  override def cloneStmt() = {
    val newStmt = DeclarationStatement(decls.map(decl => decl.cloneDecl))
    newStmt.base_copyFrom(this)
    newStmt  
  }
  
  override def pretty(): String = {
    "var " + decls.map(decl => decl.pretty()).mkString(", ")
  }
  
  override def treePretty(level: Int = 0): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("DeclarationStatement: ")
    str.append(pretty())
    str.append("\n")
    
    decls.foreach(decl => {
      str.append(decl.treePretty(level+1))
    })
    
    str.toString
  }
}


