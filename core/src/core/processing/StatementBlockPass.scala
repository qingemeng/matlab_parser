package   core.processing

import   core._
import   model._
import   model.statement._

// A simple pass to replace statement body with statement block where applicable
// To be run before any other passes because original nodes are replaced with new ones
// * Must precede all other passes
class StatementBlockPass 
  extends StatementVisitor {
  
  override def visit(stmt: ExpressionStatement): Int = {
    StatementVisitor.Continue
  }
  
  override def visit(stmt: ForStatement): Int = {
    stmt.body match {
      case s: StatementBlock => 
      case other             => stmt.update(body = StatementBlock(List(other)))
    }
    StatementVisitor.Continue
  }
  
  override def visit(stmt: IfStatement): Int = { 
    val thenBody = stmt.thenBody match {
      case s: StatementBlock => s
      case other             => StatementBlock(List(other))
    }
    if (!stmt.elseBody.isDefined) {
      stmt.update(thenBody = thenBody)  
    } else {
      val elseBody = stmt.elseBody.get match {
        case s: StatementBlock => Some(s)
        case other             => Some(StatementBlock(List(other)))
      }
      stmt.update(thenBody = thenBody, elseBody = elseBody)
    }
    StatementVisitor.Continue
  }
  
  override def visit(stmt: WhileStatement): Int = { 
    stmt.body match {
      case s: StatementBlock =>
      case other             => stmt.update(body = StatementBlock(List(other)))
    }
    StatementVisitor.Continue
  }
  
  override def visit(stmt: DoStatement): Int = { 
    stmt.body match {
      case s: StatementBlock => 
      case other             => stmt.update(body = StatementBlock(List(other)))
    }
    StatementVisitor.Continue
  }
  
  override def visit(stmt: SwitchStatement): Int = { 
    stmt.body match {
      case s: StatementBlock => 
      case other             => stmt.update(body = StatementBlock(List(other)))
    }
    StatementVisitor.Continue
  }
  
  override def visit(stmt: LabelStatement): Int = {
    stmt.nestedStmt match {
      case s: StatementBlock => 
      case other             => stmt.update(nestedStmt = StatementBlock(List(other)))
    }
    StatementVisitor.Continue
  }
}

object StatementBlockPass {
  def doPass(tu: TranslationUnit): Unit = {
    StatementProcessor.process(tu, new StatementBlockPass())
  }
  def doPass(function: FunctionDef): Unit = {
    StatementProcessor.process(function, new StatementBlockPass())
  }
}