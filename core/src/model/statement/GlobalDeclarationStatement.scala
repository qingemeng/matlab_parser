package model.statement

import   model._
import   model.statement._
import   model.expression._

object GlobalDeclarationStatement {
  def apply(scope:String,decls: Declarator) = new DeclarationStatement(List(decls))
  def apply(scope: String,decls: List[Declarator]) = {
    new GlobalDeclarationStatement(scope,decls)
  }
  def unapply(s: GlobalDeclarationStatement) = Some(s.scope,s.decls)
}

class GlobalDeclarationStatement(private val _scope:String ,private var _decls: List[Declarator]) extends Statement {

  def decls = _decls
  def scope = _scope

  def update(decls: List[Declarator] = _decls) = {
    _decls = decls
    this
  }

  // deep clone of the model.statement
  override def cloneStmt() = {
    val newStmt = GlobalDeclarationStatement(scope,decls.map(decl => decl.cloneDecl))
    newStmt.base_copyFrom(this)
    newStmt
  }

  override def pretty(): String = {
    "Scope: "+ scope+ " var " + decls.map(decl => decl.pretty()).mkString(", ")
  }

  override def treePretty(level: Int = 0): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("GlobalDeclarationStatement: ")
    str.append(pretty())
    str.append("\n")

    decls.foreach(decl => {
      str.append(decl.treePretty(level+1))
    })

    str.toString
  }
}


