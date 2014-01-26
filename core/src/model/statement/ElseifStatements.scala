//package model.statement
//
//import   model._
//import   model.statement._
//import   model.expression._
//
//import scala.collection.mutable
//
//object ElseifStatements {
//  def apply() = new ElseifStatements()
//  def apply(elseifStmts: List[ElseifStatement]) = {
//    val sb = new ElseifStatements()
//    sb.appendStatement(elseifStmts)
//  }
//  def unapply(s: ElseifStatements) = Some(s.statementBlocks)
//}
//
//class ElseifStatements() extends StatementBlock {
//
//  private val _statementBlocks = mutable.ListBuffer.empty[StatementBlock]
//
//  def statementBlocks = _statementBlocks.toList
//
//  def getNextStatementBlk(refStmtBlk: StatementBlock) = {
//    val idx = statementBlocks.indexOf(refStmtBlk)
//    if (idx < 0) null
//    if (idx >= statementBlocks.size-1) null
//    statementBlocks(idx + 1)
//  }
//
//  def getPreviousStatementBlock(refStmtblk: StatementBlock) = {
//    val idx = statements.indexOf(refStmtblk)
//    if (idx <= 0) null
//    if (idx > statements.size) null
//    statementBlocks(idx - 1)
//  }
//
//  def appendStatementBlks(stmtBlks: List[StatementBlock]) = {
//    stmtBlks.foreach(s => s.setParent(this))
//    _statementBlocks.appendAll(stmtBlks)
//    this
//  }
//
//  def appendStatementBlk(stmtBlk: StatementBlock) = {
//    stmtBlk.setParent(this)
//    _statementBlocks.append(stmtBlk)
//    this
//  }
//
//  def prependStatementBlks(stmtBlk: List[StatementBlock]) = {
//    stmtBlk.foreach(s => s.setParent(this))
//    _statementBlocks.prependAll(stmtBlk)
//    this
//  }
//
//  def prependStatementBlk(stmtBlk: StatementBlock) = {
//    stmtBlk.setParent(this)
//    _statementBlocks.prepend(stmtBlk)
//    this
//  }
//
//  def replaceStatementBlk(refStmtBlk: StatementBlock, newStmtBlk: StatementBlock) = {
//    val i = _statementBlocks.indexOf(refStmtBlk)
//    if (i != -1) {
//      newStmtBlk.setParent(this)
//      _statementBlocks.update(i, newStmtBlk)
//    }
//    this
//  }
//
//  def insertBefore(refStmtBlk: StatementBlock, newStmtBlk: StatementBlock) = {
//    val i = _statementBlocks.indexOf(refStmtBlk)
//    if (i != -1) {
//      newStmtBlk.setParent(this)
//      _statementBlocks.insert(i, newStmtBlk)
//    }
//    this
//  }
//
//  def insertAfter(refStmtBlk: StatementBlock, newStmtBlk: StatementBlock) = {
//    val i = _statementBlocks.indexOf(refStmtBlk)
//    if (i != -1) {
//      newStmtBlk.setParent(this)
//      _statementBlocks.insert(i + 1, newStmtBlk)
//    }
//    this
//  }
//
//  def removeStatement(refStmtBlk: StatementBlock) = {
//    val i = _statementBlocks.indexOf(refStmtBlk)
//    if (i != -1) _statementBlocks.remove(i)
//    this
//  }
//
//  def removeAllStatementBlks(){
//    _statementBlocks.clear()
//  }
//
//  // deep clone of the model.statement
//  override def cloneStmt() = {
//    val newStmtBlk = ElseifStatements(statementBlocks.map(stmtBlk => stmtBlk.cloneStmt))
//    newStmtBlk.statementBlocks.foreach(statementBlock => statementBlock.setParent(newStmtBlk))
//    newStmtBlk.base_copyFrom(this)
//    newStmtBlk
//  }
//
//  override def pretty() = pretty(1)
//  override def pretty(level: Int): String = {
//    val str = new StringBuilder
//    str.append(indentStr(level-1))
//    str.append("{\n")
//    for (i <- 0 until statementBlocks.size) {
//      str.append(statementBlocks(i).pretty(level))
//      //if (i != statements.size - 1)
//      str.append("\n")
//    }
//    str.append(indentStr(level-1))
//    str.append("}\n")
//    str.toString
//  }
//
//  override def treePretty(level: Int = 0): String = {
//    val str = new StringBuilder
//    str.append(indentStr(level))
//    str.append("StatementBlock: ")
//    //str.append(pretty())
//    str.append("\n")
//
//    statementBlocks.foreach(s => {
//      str.append(s.treePretty(level+1))
//    })
//
//    str.toString
//  }
//}