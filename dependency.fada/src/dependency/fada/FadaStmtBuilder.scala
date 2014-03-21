package dependency.fada

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

import  core._
import  core.processing.LoopHelper
import  model._
import  model.expression._
import  model.statement._
import com.hpctoday.fada._

import StatementType._

object FadaStmtBuilder {
  def generateProgram(stmt: DSLStatement, generateArrays: Boolean) = {
    val program = new FadaStatement()
    val stmtList = ListBuffer[DSLStatement]()    
    
    def process(s: DSLStatement){
      val visitor = new FadaStmtBuilder(generateArrays)
      StatementProcessor.process(s, visitor)
        
      if(visitor.finalStmt == null){
        System.err.println("Cannot generate statement: " + s.pretty())
      } else {
        val fadaStmt = visitor.finalStmt
    	program.Enclose(fadaStmt, false)
    	  
    	stmtList ++= visitor.stmtList
      }
    }
    
    if(stmt.isInstanceOf[StatementBlock]){
      val blk = stmt.asInstanceOf[StatementBlock]
      
      blk.statements.foreach(process(_))
    } else {
      process(stmt)
    }
    
    stmtList.zipWithIndex.foreach {
      case (s, index) => s[FadaId].id = index
    }
    
    (program, stmtList.toList)
  }
}

class FadaStmtBuilder(generateArrays: Boolean) extends StatementVisitor{
  var finalStmt: FadaStatement = null
  val stmtList = ListBuffer[DSLStatement]()
  
  val FadaExprBuilder = FadaExprBuilderFactory(generateArrays)
  
  def generateStatement(stmt: DSLStatement) = {
    val visitor = new FadaStmtBuilder(generateArrays)
    StatementProcessor.process(stmt, visitor)
    if(visitor.finalStmt == null) System.err.println("Cannot generate statement: " + stmt.pretty())
    stmtList ++= visitor.stmtList
    visitor.finalStmt
  }
  
  override def visit(stmt: StatementBlock): Int = {
    finalStmt = new FadaStatement()
    //stmtList += stmt
    
    stmt.statements.foreach(s => {
      val fadaStmt = generateStatement(s)
      if(fadaStmt != null)
    	  finalStmt.Enclose(fadaStmt, false)
	})
    
    StatementVisitor.Skip
  }
  
  override def visit(stmt: FunctionCallStatement): Int = {
    finalStmt = new FadaStatement(new Assignment(FadaExprBuilder.generateExpression(stmt.funcCallExpr)))
    stmtList += stmt
    StatementVisitor.Skip
  }
  
  //Assume only 1 declarator
  override def visit(stmt: DeclarationStatement): Int = {
    if(stmt.decls.length > 1){
      throw new UnsupportedOperationException(stmt.decls.length + " declarators in the DeclarationStatement not supported")
    }
    
	val decl = stmt.decls(0)
	
	decl.initializer match {
	  case Some(expr) => {
	    try {
			val lhs = FadaExprBuilder.generateExpression(expr)
			
			if(lhs != null){
				finalStmt = new FadaStatement(new Assignment(decl.idName.name, lhs))
				
				stmtList += stmt
			}
	    } catch {
	      case e: Throwable => System.err.println("DeclarationStatement: " + e.getMessage())
	    }
	  }
	  case None =>
	}
	
    StatementVisitor.Skip
  }
  
  override def visit(stmt: ExpressionStatement): Int = {
    try {
      finalStmt = new FadaStatement(new Assignment(FadaExprBuilder.generateExpression(stmt.expr)))
      stmtList += stmt
    } catch {
      case e: Throwable => System.err.println("ExpressionStatement: " + e.getMessage())
    }
    StatementVisitor.Skip
  }

  //Assume iterator to be 1
  override def visit(stmt: ForStatement): Int = {
    LoopHelper.hasValidForHeader(stmt) map {
      case loopInfo => {
        val forControl = new Control(loopInfo.inductionVar.name,
            FadaExprBuilder.generateExpression(loopInfo.lowerBound),
            FadaExprBuilder.generateExpression(Algebra.simplify(loopInfo.upperBound - 1)))
        val forStmt = new FadaStatement(forControl);
        stmtList += stmt
        
        stmt.body match {
          case blk: StatementBlock => blk.statements.foreach(s => {
            val fadaStmt = generateStatement(s)
            if(fadaStmt != null)
            forStmt.Enclose(fadaStmt, false)
          })
          case s => {
            val fadaStmt = generateStatement(s)
            if(fadaStmt != null)
            forStmt.Enclose(fadaStmt, false)
          }
        }    
        
        finalStmt = forStmt
      }
    }
    StatementVisitor.Skip
  }
  
  override def visit(stmt: IfStatement): Int = {
    val cond = FadaExprBuilder.generateCondition(stmt.condExpr)
    val ifControl = new Control(cond)
    val ifStmt = new FadaStatement(ifControl)
    stmtList += stmt
    
    stmt.thenBody match {
      case blk: StatementBlock => blk.statements.foreach(s => {
        val fadaStmt = generateStatement(s)
        if(fadaStmt != null)	      
    	  ifStmt.Enclose(fadaStmt, false)
      })
      case s => {
        val fadaStmt = generateStatement(s)
        if(fadaStmt != null)	      
    	  ifStmt.Enclose(fadaStmt, false)
      }
    }
    
    stmt.elseBody match {
      case Some(elseBody) => elseBody match {
	    case blk: StatementBlock => blk.statements.foreach(s => {
	      val fadaStmt = generateStatement(s)
	      if(fadaStmt != null)	      
	    	ifStmt.Enclose(fadaStmt, true)
	    })
	    case s => {
	      val fadaStmt = generateStatement(s)
	      if(fadaStmt != null)	      
	    	ifStmt.Enclose(fadaStmt, true)
	    }
      }
      case None =>
    }
    
    finalStmt = ifStmt
    StatementVisitor.Skip
  }
  
  override def visit(stmt: WhileStatement): Int = {
    val inductionVar:IdName = stmt.condExpr match {
      case e: NAryExpr => {
        e.terms(0) match {
          case e2: IdExpr => e2.idName
          case _ => null
        }
      }
      case _ => null
	}
    
    if(inductionVar != null){
      val cond = FadaExprBuilder.generateCondition(stmt.condExpr)
      val whileControl = new Control(inductionVar.name, cond)
      val whileStmt = new FadaStatement(whileControl)
      stmtList += stmt
      
      stmt.body match {
	    case blk: StatementBlock => blk.statements.foreach(s => {
	      val fadaStmt = generateStatement(s)
	      if(fadaStmt != null)	      
	    	whileStmt.Enclose(fadaStmt, false)
	    })
	    case s => {
	      val fadaStmt = generateStatement(s)
	      if(fadaStmt != null)	      
	    	whileStmt.Enclose(fadaStmt, false)
	    }
      }
      
      finalStmt = whileStmt
    }
    
    StatementVisitor.Skip    
  }
  
  override def visit(stmt: DoStatement): Int = {
    System.err.println("FadaError: " + stmt.pretty())
    StatementVisitor.Skip
  }
  
  override def visit(stmt: ReturnStatement): Int = {
    stmt.returnValue match {
      case Some(expr) => {
        val lhs = FadaExprBuilder.generateExpression(expr)
        finalStmt = new FadaStatement(new Assignment("return", lhs))
        stmtList += stmt
      }
      case None =>
    }
    StatementVisitor.Skip
  }

  override def visit(stmt: AssignmentStatement): Int = {
    val lhs = FadaExprBuilder.generateExpression(stmt.lhsExpr)
    var rhs = FadaExprBuilder.generateExpression(stmt.rhsExpr)
    
    stmt.assignOp match {
      case OpAssign() =>
	  case OpPlusAssign()	=> rhs = new Expression(lhs, Expression.Operation.FADA_ADD, rhs)
	  case OpMinusAssign()	=> rhs = new Expression(lhs, Expression.Operation.FADA_SUB, rhs)
	  case OpTimesAssign()	=> rhs = new Expression(lhs, Expression.Operation.FADA_MUL, rhs)
	  case OpDivideAssign()	=> rhs = new Expression(lhs, Expression.Operation.FADA_DIV, rhs)
      case OpModuloAssign() => rhs = new Expression(lhs, Expression.Operation.FADA_MOD, rhs)
	  
	  //case OpShiftLeftAssign() => 
	  //case OpShiftRightAssign() => 
	  //case OpBinaryAndAssign() => 
	  //case OpBinaryXorAssign() => 
	  //case OpBinaryOrAssign() =>
	  case _ =>
    }
    
    stmt.lhsExpr match {
      case expr: IdExpr => {
        finalStmt = new FadaStatement(new Assignment(expr.idName.name, rhs))
      }
      case expr: ArrayRefExpr => {
    	val indices = expr.indices.map(index => FadaExprBuilder.generateExpression(index))
    	val arrayName = expr.owner.asInstanceOf[IdExpr].idName.name
    	finalStmt = new FadaStatement(new Assignment(arrayName, new FADA_Index(indices), rhs))
      }
    }
    stmtList += stmt
    StatementVisitor.Skip
  }
}