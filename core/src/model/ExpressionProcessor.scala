package   model

import scala.collection.mutable.ListBuffer

import   model._
import   model.expression._
import   model.statement._


object ExpressionProcessor {
  def process(expr: Expr, visitor: ExpressionVisitor): Expr = {
    new ExpressionProcessor(visitor).process(expr)
  }
  def process(tu: TranslationUnit, visitor: ExpressionVisitor): Unit = {
    new StatementProcessor(ExpressionStatementProcessor(visitor)).process(tu)
  }
  def process(function: FunctionDef, visitor: ExpressionVisitor): Unit = {
    new StatementProcessor(ExpressionStatementProcessor(visitor)).process(function)
  }
  def process(statement: Statement, visitor: ExpressionVisitor): Unit = {
    new StatementProcessor(ExpressionStatementProcessor(visitor)).process(statement)
  }
}

class ExpressionProcessor(private val visitor: ExpressionVisitor) {

  def process(expr: Expr): Expr = {
    val result = visitor.visit(expr)
    if (result == ExpressionVisitor.Continue) {
      val xpr = expr match {
        case e: NAryExpr            => process(e)
        case e: UnaryExpr           => process(e)
        case e: ArrayRefExpr        => process(e)
        case e: ArrayEndExpr        => process(e)
        case e: FieldRefExpr        => process(e)
        case e: IdExpr              => process(e)
        case e: FunctionCallExpr    => process(e)
        case e: ExpressionListExpr  => process(e)
        case e: TupleExpr  		      => process(e)
//        case e: VectorExpr          => process(e)
        case e: ConditionalExpr     => process(e)
        case e: TypeIdExpr          => process(e)
        case e: ConstLiteralExpr    => process(e)
        case e: SliceExpr           => process(e)
        case e: AllocateArrayExpr   => process(e)
        case e: DeallocateArrayExpr => process(e)
        case e: ArrayCompositionExpr    => process(e)
      }
      
      if(xpr != null){
        visitor.leave(xpr)
      } else {
        null
      }
    } else if (result == ExpressionVisitor.Skip){
      visitor.leave(expr)
    } else {
      null
    }
  }

  def process(expr: NAryExpr): Expr = {
    val result = visitor.visit(expr)
    if (result == ExpressionVisitor.Continue) {
      val terms = ListBuffer.empty[Expr]
      val pass = expr.terms.forall { e => 
        val xpr = process(e)
        if (xpr != null) terms += xpr
        xpr != null
      }
      if (!pass) return null 
      expr.update(terms = terms.toList)
    }
    if (result != ExpressionVisitor.Abort) visitor.leave(expr)
    else null
  }
  
  def process(expr: UnaryExpr): Expr = {
    val result = visitor.visit(expr)
    if (result == ExpressionVisitor.Continue) {
      val term = process(expr.term)
      if (term == null) return null 
      expr.update(term = term)
    }
    if (result != ExpressionVisitor.Abort) visitor.leave(expr)
    else null
  }
  
  def process(expr: ArrayRefExpr): Expr = {
    val result = visitor.visit(expr)    
    if (result == ExpressionVisitor.Continue) {
      val owner = process(expr.owner)
      if (owner == null) return null
      
      val indices = ListBuffer.empty[Expr]
      val pass = expr.indices.forall { e => 
        val xpr = process(e)
        if (xpr != null) indices += xpr
        xpr != null
      }
      if (!pass) return null 
      expr.update(owner = owner, indices = indices.toList)
    }
    if (result != ExpressionVisitor.Abort) visitor.leave(expr)
    else null
  }
  
  def process(expr: FieldRefExpr): Expr = {
    val result = visitor.visit(expr)    
    if (result == ExpressionVisitor.Continue) {
      val owner = process(expr.owner)
      if (owner == null) return null
      expr.update(owner = owner)
    }
    if (result != ExpressionVisitor.Abort) visitor.leave(expr)
    else null
  }
  
  def process(expr: IdExpr): Expr = {
    val result = visitor.visit(expr)    
    if (result == ExpressionVisitor.Abort) return null
    visitor.leave(expr)
  }
  
  def process(expr: FunctionCallExpr): Expr = {
    val result = visitor.visit(expr)    
    if (result == ExpressionVisitor.Continue) {
      val funcNameExpr = process(expr.funcNameExpr)
      if (funcNameExpr == null) return null
      val params = ListBuffer.empty[Expr]
      val pass = expr.params.forall { e => 
        val xpr = process(e)
        if (xpr != null) params += xpr
        xpr != null
      }
      if (!pass) return null 
      expr.update(funcNameExpr = funcNameExpr, params = params.toList)
    }
    if (result != ExpressionVisitor.Abort) visitor.leave(expr)
    else null
  }
  
  def process(expr: ExpressionListExpr): Expr = {
    val result = visitor.visit(expr)    
    if (result == ExpressionVisitor.Continue) {
      val exprs = ListBuffer.empty[Expr]
      val pass = expr.exprs.forall { e => 
        val xpr = process(e)
        if (xpr != null) exprs += xpr
        xpr != null
      }
      if (!pass) return null 
      expr.update(exprs = exprs.toList)
    }
    if (result != ExpressionVisitor.Abort) visitor.leave(expr)
    else null
  }
  
  def process(expr: TupleExpr): Expr = {
    val result = visitor.visit(expr)    
    if (result == ExpressionVisitor.Continue) {
      val exprs = ListBuffer.empty[Expr]
      val pass = expr.exprs.forall { e => 
        val xpr = process(e)
        if (xpr != null) exprs += xpr
        xpr != null
      }
      if (!pass) return null 
      expr.update(exprs = exprs.toList)
    }
    if (result != ExpressionVisitor.Abort) visitor.leave(expr)
    else null
  }

//  def process(expr: VectorExpr): Expr ={
//    val result = visitor.visit(expr)
//    if( result ==ExpressionVisitor.Continue){
//      val exprs  = ListBuffer.empty[Expr]
//      val pass = expr.exprs.forall{e=>
//        val xpr = process(e)
//        if(xpr != null ) exprs +=xpr
//        xpr != null
//      }
//      if (!pass) return null
//      expr.update(exprs = exprs.toList)
//    }
//    if(result !=ExpressionVisitor.Abort)
//      visitor.leave(expr)
//    else null
//  }
  
  def process(expr: ArrayCompositionExpr): Expr = {
    val result = visitor.visit(expr)    
    if (result == ExpressionVisitor.Continue) {
      val exprs = ListBuffer.empty[Expr]
      val pass = expr.exprs.forall { e => 
        val xpr = process(e)
        if (xpr != null) exprs += xpr
        xpr != null
      }
      if (!pass) return null 
      expr.update(exprs = exprs.toList)
    }
    if (result != ExpressionVisitor.Abort) visitor.leave(expr)
    else null
  }

  def process(expr: ArrayEndExpr): Expr = {
    val result = visitor.visit(expr)    
    if (result == ExpressionVisitor.Continue) {
    }
    if (result != ExpressionVisitor.Abort) visitor.leave(expr)
    else null
  }
  
  def process(expr: ConditionalExpr): Expr = {
    val result = visitor.visit(expr)    
    if (result == ExpressionVisitor.Continue) {
      val condExpr = process(expr.condExpr)
      if (condExpr == null) return null
      val positiveExpr = process(expr.positiveExpr)
      if (positiveExpr == null) return null
      val negativeExpr = process(expr.negativeExpr)
      if (negativeExpr == null) return null
      expr.update(condExpr = condExpr, positiveExpr = positiveExpr, negativeExpr = negativeExpr)
    }
    if (result != ExpressionVisitor.Abort) visitor.leave(expr)
    else null
  }
  
  def process(expr: TypeIdExpr): Expr = {
    val result = visitor.visit(expr)    
    if (result == ExpressionVisitor.Abort) return null
    visitor.leave(expr)
  }
  
  def process(expr: ConstLiteralExpr): Expr = {
    val result = visitor.visit(expr)    
    if (result == ExpressionVisitor.Abort) return null
    visitor.leave(expr)
  }

  def process(expr: SliceExpr): Expr = {
    val result = visitor.visit(expr)    
    if (result == ExpressionVisitor.Continue) {
      val lowerBound = process(expr.lowerBound)
      if(lowerBound == null) return null
      
      val upperBound = process(expr.upperBound)
      if(upperBound == null) return null
      
      val stride = process(expr.stride)
      if(stride == null) return null
      
      expr.update(lowerBound, upperBound, stride)
    }
    
    if (result == ExpressionVisitor.Abort) return null
    visitor.leave(expr)
  }

  def process(expr: AllocateArrayExpr) : Expr = {
    val result = visitor.visit(expr)
    if (result == ExpressionVisitor.Continue) {
	  val arrayInfo = expr.arrayInfo
	  val newSizes = ListBuffer.empty[Expr]
	  val pass = arrayInfo.sizes.forall { e => 
	    val xpr = process(e)
	    if (xpr != null) newSizes += xpr
	    xpr != null
	  }
	  if (!pass) return null
	  
	  val newInfo = ArrayInfo(arrayInfo.arrayType, arrayInfo.rank, newSizes.toList)
	  expr.update(newInfo)
    }
    if (result == ExpressionVisitor.Abort) return null
    visitor.leave(expr)
  }
  
  def process(expr: DeallocateArrayExpr) : Expr = {
    val result = visitor.visit(expr)    
    if (result == ExpressionVisitor.Abort) return null
    visitor.leave(expr)
  }  
}