package   model

import   model._
import   model.expression._

import scala.collection.mutable.ListBuffer

object ExpressionVisitor {
  val Skip = 0
  val Continue = 1
  val Abort = 2
}

abstract class ExpressionVisitor {
  def visit(expr: Expr): Int = { ExpressionVisitor.Continue }
  
  def visit(expr: NAryExpr): Int = { ExpressionVisitor.Continue }
  def visit(expr: UnaryExpr): Int = { ExpressionVisitor.Continue }
  def visit(expr: ArrayRefExpr): Int = { ExpressionVisitor.Continue }
  def visit(expr: FieldRefExpr): Int = { ExpressionVisitor.Continue }
  def visit(expr: IdExpr): Int = { ExpressionVisitor.Continue }
  def visit(expr: ConstLiteralExpr): Int = { ExpressionVisitor.Continue }
  def visit(expr: ArrayCompositionExpr): Int = { ExpressionVisitor.Continue }
  def visit(expr: ArrayEndExpr): Int = { ExpressionVisitor.Continue }
  def visit(expr: FunctionCallExpr): Int = { ExpressionVisitor.Continue }
  def visit(expr: ExpressionListExpr): Int = { ExpressionVisitor.Continue }
  def visit(expr: TupleExpr): Int = { ExpressionVisitor.Continue }
  //def visit(expr: VectorExpr): Int = {ExpressionVisitor.Continue}
  def visit(expr: ConditionalExpr): Int = { ExpressionVisitor.Continue }
  def visit(expr: TypeIdExpr): Int = { ExpressionVisitor.Continue }
  def visit(expr: SliceExpr): Int = { ExpressionVisitor.Continue }
  def visit(expr: AllocateArrayExpr): Int = { ExpressionVisitor.Continue }
  def visit(expr: DeallocateArrayExpr): Int = { ExpressionVisitor.Continue }

  def leave(expr: Expr): Expr = { expr }
  def leave(expr: NAryExpr): Expr = { expr }
  def leave(expr: UnaryExpr): Expr = { expr }
  def leave(expr: ArrayRefExpr): Expr = { expr }
  def leave(expr: FieldRefExpr): Expr = { expr }
  def leave(expr: IdExpr): Expr = { expr }
  def leave(expr: ConstLiteralExpr): Expr = { expr }  
  def leave(expr: ArrayCompositionExpr): Expr = { expr }
  def leave(expr: ArrayEndExpr): Expr = { expr }
  def leave(expr: FunctionCallExpr): Expr = { expr }
  def leave(expr: ExpressionListExpr): Expr = { expr }
  def leave(expr: TupleExpr): Expr = { expr }
  //def leave(expr: VectorExpr): Expr = {expr}
  def leave(expr: ConditionalExpr): Expr = { expr }
  def leave(expr: TypeIdExpr): Expr = { expr }
  def leave(expr: SliceExpr): Expr = { expr }
  def leave(expr: AllocateArrayExpr): Expr = { expr }
  def leave(expr: DeallocateArrayExpr): Expr = { expr }
}