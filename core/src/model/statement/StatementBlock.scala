package model.statement

import   model._
import   model.statement._
import   model.expression._

import scala.collection.mutable

object StatementBlock {
  def apply() = new StatementBlock()
  def apply(statements: List[Statement]) = {
    val sb = new StatementBlock()
    sb.appendStatements(statements)
  }
  def unapply(s: StatementBlock) = Some(s.statements)
}

class StatementBlock() extends Statement {
  
  private val _statements = mutable.ListBuffer.empty[Statement]
  
  // constant time conversion
  def statements = _statements.toList
  
  def getNextStatement(refStmt: Statement) = {
    val idx = statements.indexOf(refStmt)
    if (idx < 0) null
    if (idx >= statements.size-1) null
    statements(idx + 1)
  }

  def getPreviousStatement(refStmt: Statement) = {
    val idx = statements.indexOf(refStmt)
    if (idx <= 0) null
    if (idx > statements.size) null
    statements(idx - 1)
  }
  
  def appendStatements(stmt: List[Statement]) = {
    stmt.foreach(s => s.setParent(this))
    _statements.appendAll(stmt)
    this
  }
  
  def appendStatement(stmt: Statement) = {
    stmt.setParent(this)
    _statements.append(stmt)
    this
  }
  
  def prependStatements(stmt: List[Statement]) = {
    stmt.foreach(s => s.setParent(this))
    _statements.prependAll(stmt)
    this
  }

  def prependStatement(stmt: Statement) = {
    stmt.setParent(this)
    _statements.prepend(stmt)
    this
  }

  def replaceStatement(refStmt: Statement, newStmt: Statement) = {
    val i = _statements.indexOf(refStmt)
    if (i != -1) {
      newStmt.setParent(this)
      _statements.update(i, newStmt)
    }
    this
  }
  
  def insertBefore(refStmt: Statement, newStmt: Statement) = {
    val i = _statements.indexOf(refStmt)
    if (i != -1) {
      newStmt.setParent(this)
      _statements.insert(i, newStmt)
    }
    this
  }
  
  def insertAfter(refStmt: Statement, newStmt: Statement) = {
    val i = _statements.indexOf(refStmt)
    if (i != -1) {
      newStmt.setParent(this)
      _statements.insert(i + 1, newStmt)
    }
    this
  }
  
  def removeStatement(refStmt: Statement) = {
    val i = _statements.indexOf(refStmt)
    if (i != -1) _statements.remove(i)
    this
  }
  
  def removeAllStatements(){
    _statements.clear()
  }
  
  // deep clone of the model.statement
  override def cloneStmt() = {
    val newStmt = StatementBlock(statements.map(stmt => stmt.cloneStmt))
    newStmt.statements.foreach(stmt => stmt.setParent(newStmt))
    newStmt.base_copyFrom(this)
    newStmt
  }
  
  override def pretty() = pretty(1)
  override def pretty(level: Int): String = {
    val str = new StringBuilder
    str.append(indentStr(level-1))
    str.append("{\n")
    for (i <- 0 until statements.size) {
      str.append(statements(i).pretty(level))
      //if (i != statements.size - 1)
      str.append("\n")
    }
    str.append(indentStr(level-1))
    str.append("}\n")
    str.toString
  }
  
  override def treePretty(level: Int = 0): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("StatementBlock: ")
    //str.append(pretty())
    str.append("\n")
    
    statements.foreach(s => {
      str.append(s.treePretty(level+1))
    })

    str.toString
  }

  override def typePretty(level: Int = 0): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("StatementBlock: ")
    //str.append(pretty())
    str.append("\n")

    statements.foreach(s => {
      str.append(s.typePretty(level+1))
    })

    str.toString
  }

}