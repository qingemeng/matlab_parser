package model.expression

import   model._
import   model.expression._
import   model.property._
import scala.util.parsing.input.Positional

//import refactoring.matlab.processing.SimpleTypeInferencer_
import scala.collection.mutable

abstract class BaseExpr

abstract class Expr extends BaseExpr with HasProperties with Positional {

  private var _parent: Option[Expr] = None
//  private var _position = this.pos

  //  var _typeInfo: Map[String, BasicType] = Map.empty


  // Pretty Print
  protected def indentStr(level: Int): String = "  " * level
  def pretty(hash: Boolean = false): String
  def treePretty(level: Int = 0, hash: Boolean = false): String
  def semanticAnalyse(level: Int = 0, hash: Boolean = false): String
  
  def parent = _parent
  def getParent = _parent.getOrElse(null)
  def setParent(p: Expr) = if (p == null) _parent = None else _parent = Some(p)
//  def position = _position
//  def getPosition = _position
//  def setPosition (pos:Positional) = if (pos == null) _position = null else _position = this.pos

  // deep clone of the model.expression
  def cloneExpr(): Expr
  
  def isLValue(): Boolean = false
  def isLeafNode(): Boolean = false
  
  // compares if expressions are identical
  // The "==" operator uses equals and hashCode
  override def equals(that: Any): Boolean
  override def hashCode(): Int = pretty(true).hashCode
  
  // copy attributes from given model.expression to this model.expression
  def base_copyFrom(expr: Expr): Expr = {
    this.setParent(expr.getParent)
    this.copyProperties(expr)
    this
  }
  
  // some convenient model.expression builders
  def +(literal: Int): Expr = NAryExpr(OpPlus(), List(this, ConstLiteralExpr(literal)))
  def +(literal: Float): Expr = NAryExpr(OpPlus(), List(this, ConstLiteralExpr(literal)))
  def +(expr: Expr): Expr = NAryExpr(OpPlus(), List(this, expr))
  //def -(literal: Int): Expr = NAryExpr(OpPlus(), List(this, UnaryExpr(OpNegate(), ConstLiteralExpr(literal))))
  //def -(literal: Float): Expr = NAryExpr(OpPlus(), List(this, UnaryExpr(OpNegate(), ConstLiteralExpr(literal))))
  //def -(expr: Expr): Expr = NAryExpr(OpPlus(), List(this, UnaryExpr(OpNegate(), expr)))
  def -(literal: Int): Expr = NAryExpr(OpPlus(), List(this, NAryExpr(OpTimes(), List(ConstLiteralExpr(-1), ConstLiteralExpr(literal)))))
  def -(literal: Float): Expr = NAryExpr(OpPlus(), List(this, NAryExpr(OpTimes(), List(ConstLiteralExpr(-1), ConstLiteralExpr(literal)))))
  def -(expr: Expr): Expr = NAryExpr(OpPlus(), List(this, NAryExpr(OpTimes(), List(ConstLiteralExpr(-1), expr))))
  def *(literal: Int): Expr = NAryExpr(OpTimes(), List(this, ConstLiteralExpr(literal)))
  def *(literal: Float): Expr = NAryExpr(OpTimes(), List(this, ConstLiteralExpr(literal)))
  def *(expr: Expr): Expr = NAryExpr(OpTimes(), List(this, expr))
  def /(literal: Int): Expr = NAryExpr(OpDivide(), List(this, ConstLiteralExpr(literal)))
  def /(literal: Float): Expr = NAryExpr(OpDivide(), List(this, ConstLiteralExpr(literal)))
  def /(expr: Expr): Expr = NAryExpr(OpDivide(), List(this, expr))

//  typeInfo = SimpleTypeInferencer.infer(ast)

  def generateTypeStr(t: BasicType) = t match {
    case t: IntType    => "int"
    case t: FloatType  => "float"
    case t: DoubleType => "double"
    case t: BooleanType => "boolean"
  }


//  def generateVarWithType: mutable.ListBuffer[String] = {
//    val gen = mutable.ListBuffer.empty[String]
//    _typeInfo.foreach { case (id, iType) =>
//      if (iType != null) {
//        val decl = iType match {
//          case t: IntType    => id + " : " + generateTypeStr(t)
//          case t: FloatType  => id + " : "  + generateTypeStr(t)
//          case t: DoubleType => id + " : "  + generateTypeStr(t)
//          case t: BooleanType =>id + " : "  + generateTypeStr(t)
//          case t: ArrayType  => s"NdArray<${generateTypeStr(t.subType)}> $id"
//        }
//        gen += decl
//      }
//    }

//    gen
//  }


  //def unary_-(expr: Expr) = UnaryExpr(OpNegate(), expr)
  
  // map and transform this model.expression tree (depth first)
  // leave out ExpressionListExpr and ConditionalExpr
  // Note: certain parts of the tree are not mapped or traversed, unlike ExpressionVisitor
  // e.g. array and field owners are not traversed
  
//  @deprecated
//  def mapExprTree(f: Expr => Expr): Expr = this match {
//    case e: NAryExpr         => f(e.update(terms = e.terms.map(_.mapExprTree(f))))
//    case e: UnaryExpr        => f(e.update(term = e.term.mapExprTree(f)))
//    case e: ArrayRefExpr     => f(e)
//    case e: FieldRefExpr     => f(e)
//    case e: IdExpr           => f(e)
//    case e: ConstLiteralExpr => f(e)
//    case e: FunctionCallExpr => f(e.update(params = e.params.map(_.mapExprTree(f))))
//    case e: TypeIdExpr       => f(e)
//    case e: LinSpaceExpr     => f(e)
//    case e: LoopCountExpr    => f(e)
//    case e: OnesExpr         => f(e)
//  }
}

object Expr {
  /*
  // map and transform this model.expression tree (depth first)
  // note that for the internal nodes (UnaryExpr and NAryExpr), 
  // we can choose to create a new copy or modify the existing one
  def mapExprTree(f: Expr => Expr, cloneInternalNode: Boolean = false): Expr = this match {
    case e @ UnaryExpr(_, term)      =>
      val newNode = if (cloneInternalNode) e.cloneExpr().asInstanceOf[UnaryExpr] else e
      f(newNode.update(term = term.mapExprTree(f, cloneInternalNode)))
    case e @ NAryExpr(_, terms)      =>
      val newNode = if (cloneInternalNode) e.cloneExpr().asInstanceOf[NAryExpr] else e      
      f(newNode.update(terms = terms.map(_.mapExprTree(f, cloneInternalNode))))
    case e: ArrayRefExpr             => f(e)
    case e: IdExpr                   => f(e)
    case e: ConstLiteralExpr         => f(e)
    case e: OnesExpr                 => f(e)
    case e: LoopCountExpr            => f(e)
    case e: LinSpaceExpr             => f(e)
  }*/

  /*
  def traverseTopDown[B](acc: B)(f: (B, Expr) => B): B = this match {
    case e @ UnaryExpr(_, term) =>
      val acc1 = f(acc, e)
      term.traverseTopDown(acc1)(f)
    case e @ NAryExpr(_, terms) =>
      var acc1: B = acc
      var acc2: B = acc
      for (t <- terms) {
        acc1 = f(acc1, t)
        acc2 = t.traverseTopDown(acc1)(f);
        acc1 = acc2
      }
      acc1
    case e: ArrayRefExpr             => f(acc, e)
    case e: IdExpr                   => f(acc, e)
    case e: ConstLiteralExpr[AnyVal] => f(acc, e)
    case e: LoopCountExpr            => f(acc, e)
    case e: LinSpaceExpr             => f(acc, e)
  }*/

  /*def deepClone(expr: Expr): Expr = expr match {
    case e: NAryExpr                 => e
    case e: UnaryExpr                => e
    case e: ArrayRefExpr             => e.cloneExpr
    case e: FieldRefExpr             => e.cloneExpr
    case e: IdExpr                   => e.cloneExpr
    case e: ConstLiteralExpr         => e.cloneExpr
    case e: FunctionCallExpr         => e.cloneExpr
    case e: ExpressionListExpr       => e.cloneExpr
    case e: ConditionalExpr          => e.cloneExpr
    case e: TypeIdExpr               => e.cloneExpr
    case e: OnesExpr                 => e.cloneExpr
    case e: LoopCountExpr            => e.cloneExpr
    case e: LinSpaceExpr             => e.cloneExpr
  }*/

  /*def deepClone: Expr = {
    def cloneFunc(xpr: Expr) = xpr match {
      case e: IdExpr                   => e.cloneExpr
      case e: ConstLiteralExpr         => e.cloneExpr
      case e: OnesExpr                 => e.cloneExpr
      case e: LoopCountExpr            => e.cloneExpr
      case e: LinSpaceExpr             => e.cloneExpr
      case e: ArrayRefExpr             => e.cloneExpr
      case e: UnaryExpr                => e
      case e: NAryExpr                 => e
    }

    this.mapExprTree(cloneFunc, cloneInternalNode = true)
  }*/
  
}

