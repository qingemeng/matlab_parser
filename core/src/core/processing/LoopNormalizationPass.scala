package   core.processing

import   model._
import   model.statement._
import   model.expression._

// Identifies countable loops which have initial, final and invariant stride values
// Ensures that loop body statements can be handled
// - basic types
// - AssignmentStatement
// - no return, break, continue
// - no control dependences
// Finally replaces loop with CountableLoopStatement
// Loop
// CLOOP(i = startexpr; i < endexpr; i+=n) OR
// CLOOP(i = startexpr; i > endexpr; i-=n)?
//
// * Must be preceded by AssignmentStatementPass
class LoopNormalizationPass extends StatementVisitor {
  
  
  override def leave(stmt: ForStatement): Unit = {
    
  }
}

object LoopNormalizationPass {
  def doPass(tu: TranslationUnit): Unit = {
    StatementProcessor.process(tu, new LoopNormalizationPass())
  }
  def doPass(function: FunctionDef): Unit = {
    StatementProcessor.process(function, new LoopNormalizationPass())
  }
}