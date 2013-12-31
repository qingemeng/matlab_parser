package   core.processing

import   model._
import   model.statement._
import scala.collection.mutable.ListBuffer

// Merge nested statement block to single block
object MergeNestedStatementBlock {
  def doPass(tu: TranslationUnit){
    tu.functions.foreach(doPass(_))
  }
  
  def doPass(function: FunctionDef){
    doPass(function.body)
  }
  
  def doPass(statement: Statement){
    val blocks = ListBuffer[StatementBlock]()
    
    StatementProcessor.process(statement, new StatementVisitor(){
      override def leave(stmt: StatementBlock){
    	blocks += stmt
      }
    })
    
    for(block <- blocks){
      block.parent match {
        //If parent is statement block, put all statements to parent 
        case Some(parent) => parent match {
          case parentBlock: StatementBlock => {
            for(stmt <- block.statements){
              parentBlock.insertBefore(block, stmt)
            }
            parentBlock.removeStatement(block)
          }
          case _ =>
        }
        case None =>
      }
    }
  }
}