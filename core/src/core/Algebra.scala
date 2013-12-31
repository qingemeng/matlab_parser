package   core

import model._
import   model.expression._
import scala.collection.mutable.ListBuffer
import   core.processing.NormalizeExprVisitor
import   core.processing.DenormalizeExprVisitor

// Performs symbolic algebraic manipulation of expressions
// Assumptions:
// * No assignment operators
// * OpMinus and OpDivide has been normalized
// * Currently supports 2 types of normalization for OpMinus: e.g. x - y becomes x + (-y) or x + (-1 * y)
// * Expressions has no side effects, no prefix or postfix inc/dec 
// * No ExpressionListExpr and ConditionalExpr
// Note some stored IASTNodes may be lost
object Algebra {
//  def isEqual(expr1: Expr, expr2: Expr): Option[Boolean] = {
//    simplify(expr1 - expr2) match {
//      case e: ConstLiteralExpr => Some(e.getFloat == 0)
//      case _ => None
//    }  
//  }
//  def isLess(expr1: Expr, expr2: Expr): Option[Boolean] = {
//    simplify(expr1 - expr2) match {
//      case e: ConstLiteralExpr => Some(e.getFloat < 0)
//      case _ => None
//    }  
//  }
//  def isLessEqual(expr1: Expr, expr2: Expr): Option[Boolean] = {
//    simplify(expr1 - expr2) match {
//      case e: ConstLiteralExpr => Some(e.getFloat <= 0)
//      case _ => None
//    }  
//  }
//  def isGreater(expr1: Expr, expr2: Expr): Option[Boolean] = {
//    simplify(expr1 - expr2) match {
//      case e: ConstLiteralExpr => Some(e.getFloat > 0)
//      case _ => None
//    }  
//  }
//  def isGreaterEqual(expr1: Expr, expr2: Expr): Option[Boolean] = {
//    simplify(expr1 - expr2) match {
//      case e: ConstLiteralExpr => Some(e.getFloat >= 0)
//      case _ => None
//    }  
//  }
  
  // Expand whole expression bottom-up and merge operators
  def expand(expr: Expr): Expr = {
    mergeOperators(expandExpr(expr))
  }
  
  // Bottom up expansion of expression
  def expandExpr(expr: Expr): Expr =
    ExpressionProcessor.process(expr, new ExpressionVisitor() {
      override def leave(expr: UnaryExpr): Expr = {
        expr.op match {
          case OpNegate() => expr.term match {
            case e: ConstLiteralExpr =>
              if(e.isNumeric) ConstLiteralExpr(-e.numeric)
              else {
                val c = ConstLiteralExpr(-1)
                multiplyConsts(List(c, e))
              }
            case e: IdExpr => expr
            case e @ NAryExpr(OpPlus(), _) =>
              val c = ConstLiteralExpr(-1)
              distribute(c, e)
          }
          case OpInvert() => expr.term match {
            case e: ConstLiteralExpr => {
              if(e.isNumeric && e.numeric == 1){
                ConstLiteralExpr(1)
              } else {
                ConstLiteralExpr(1 / e.numeric.toFloat)
              }
            }
            case _ => throw new UnsupportedOperationException("Invert: " + expr.term.pretty())
          }
          case _ => throw new UnsupportedOperationException(expr.op.toString)
        }
      }
      
      override def leave(expr: NAryExpr): Expr = {
        expr.op match {
          case OpTimes() => 
            // Find all non-leaf nodes
            val (leafNodes, internalNodes) = expr.terms.partition(term => term.isLeafNode())
            if (internalNodes.isEmpty) {
              expr
            } else {
              val init: Expr = NAryExpr(OpTimes(), leafNodes)
              // Distribute inner terms repeatedly for set of expressions
              internalNodes.foldLeft(init)((acc, t) => distribute(acc, t))
            }
          case OpPlus() => expr
        }
      }
    })
  
  // Apply distributive property of multiplication for Local level
  // Each of expr1 and expr2 should already be expanded
  def distribute(expr1: Expr, expr2: Expr): Expr = {
    def checkNegate(xpr: UnaryExpr) = {
      if (xpr.op != OpNegate()) throw new UnsupportedOperationException("Negate expected")
      xpr.term match {
        case e: ConstLiteralExpr =>
        case e: UnaryExpr => throw new UnsupportedOperationException("Invalid expr")
        case e: NAryExpr  => throw new UnsupportedOperationException("Invalid expr")
      }
    }
    
    expr1 match {
      case e1: ConstLiteralExpr => expr2 match {
        case e2: ConstLiteralExpr => multiplyConsts(List(e1, e2))
        case e2: IdExpr           => NAryExpr(OpTimes(), List(e1, e2))
        case e2 @ UnaryExpr(OpNegate(), _) => checkNegate(e2); UnaryExpr(e2.op, distribute(e1, e2.term))
        case e2: NAryExpr         => e2.op match {
          case OpTimes() => NAryExpr(OpTimes(), e1::e2.terms)
          case OpPlus()  => NAryExpr(OpPlus(), e2.terms.map(distribute(e1, _)))
        }
      }
      case e1: IdExpr => expr2 match {
        case e2: ConstLiteralExpr => NAryExpr(OpTimes(), List(e2, e1))
        case e2: IdExpr           => NAryExpr(OpTimes(), List(e1, e2))
        case e2 @ UnaryExpr(OpNegate(), _) => checkNegate(e2); UnaryExpr(e2.op, distribute(e1, e2.term))
        case e2: NAryExpr  => e2.op match {
          case OpTimes() => NAryExpr(OpTimes(), e1::e2.terms)
          case OpPlus()  => NAryExpr(OpPlus(), e2.terms.map(distribute(e1, _)))
        }
      } 
      case e1 @ NAryExpr(OpTimes(), _) => expr2 match {
        case e2: ConstLiteralExpr => NAryExpr(OpTimes(), e1.terms :+ e2)
        case e2: IdExpr           => NAryExpr(OpTimes(), e1.terms :+ e2)
        case e2 @ UnaryExpr(OpNegate(), _) => 
          checkNegate(e2); UnaryExpr(e2.op, distribute(e1, e2.term))
        case e2 @ NAryExpr(OpTimes(), _) => NAryExpr(OpTimes(), e1.terms:::e2.terms)
        case e2 @ NAryExpr(OpPlus(), _)  => NAryExpr(OpPlus(), e2.terms.map(distribute(e1, _)))
      }
      case e1 @ NAryExpr(OpPlus(), _) => expr2 match {
        case e2: ConstLiteralExpr => NAryExpr(OpPlus(), e1.terms.map(distribute(_, e2)))
        case e2: IdExpr           => NAryExpr(OpPlus(), e1.terms.map(distribute(_, e2)))
        case e2 @ UnaryExpr(OpNegate(), _) => 
          checkNegate(e2); UnaryExpr(e2.op, distribute(e1, e2.term))
        case e2 @ NAryExpr(OpTimes(), _) => NAryExpr(OpPlus(), e1.terms.map(distribute(_, e2)))
        case e2 @ NAryExpr(OpPlus(), _)  => 
          // this will return plus[collection of plus terms], need to merge later
          NAryExpr(OpPlus(), e1.terms.map(distribute(_, e2)))
      } 
    }
  }
  
//  private def isLeafNode(expr: Expr): Boolean = {
//    expr match {
//      case e: IdExpr           => true
//      case e: ConstLiteralExpr => true
//      case e: ArrayRefExpr     => true
//      case e: UnaryExpr        => isLeafNode(e)
//      case e: NAryExpr         => false
//    }
//  }
  
  // Merge similar children operators with parent
  def mergeOperators(expr: Expr): Expr = 
    ExpressionProcessor.process(expr, new ExpressionVisitor() {
      
      def canMergeOperator(op: NAryOp): Boolean = op match {
        case OpPlus()       => true
        case OpTimes()      => true
        case OpOuterProd()  => true
        case OpMatProd()    => true
        case OpBinaryAnd()  => true
        case OpBinaryXor()  => true
        case OpBinaryOr()   => true
        case OpLogicalAnd() => true
        case OpLogicalOr()  => true
        case _              => false
      }
      
      override def leave(expr: NAryExpr): Expr = {
        if (canMergeOperator(expr.op)) {
          val terms = ListBuffer.empty[Expr]
          for (t <- expr.terms) {
            t match {
              case e: NAryExpr => if (expr.op == e.op) terms ++= e.terms else terms += e
              case other => terms += other
            }
          }
          expr.update(terms = terms.toList)
          expr.op match {
            case OpPlus()      => if (expr.terms.size == 1) expr.terms(0) else expr
            case OpTimes()     => if (expr.terms.size == 1) expr.terms(0) else expr
            case OpOuterProd() => if (expr.terms.size == 1) expr.terms(0) else expr
            case _ => if (expr.terms.size == 1) throw new UnsupportedOperationException(expr.op.toString) else expr
          }
        } else {
          expr
        }
      }
    })
    
  // substitute IdExpr of a given IdName with expression given in map
  def substitute(expr: Expr, idMap: Map[IdName, Expr]): Expr = 
    ExpressionProcessor.process(expr, new ExpressionVisitor() {
      override def leave(expr: IdExpr): Expr = {
        if (idMap.contains(expr.idName)) idMap(expr.idName)
        else expr
      }
    })
    
  // Must expand whole expression so as to handle Unary -(...)
//  def simplify(expr: Expr): Expr = {
//	  simplifyMerged(expand(mergeOperators(expr)))
//  }
  def simplify(expr: Expr) = {
    val normVisitor = new NormalizeExprVisitor()
    val denormVisitor = new DenormalizeExprVisitor()
    var xpr = ExpressionProcessor.process(expr, normVisitor)
    xpr = simplifyMerged(expand(mergeOperators(xpr)))
    xpr = ExpressionProcessor.process(xpr, denormVisitor)
    xpr
  }  
  
  // Collect terms and simplify them by coalescing, applied from bottom up
  // mergeOperators should be called prior to simplify
  // Does not expand whole expression
  def simplifyMerged(expr: Expr): Expr =
    ExpressionProcessor.process(expr, new ExpressionVisitor() {
      // add the similar terms in list
      private def simplifyAddition(exprs: List[Expr]): Expr = {
        // perform addition and find numeric kind
        // note that exprs should be same type
        var total = NumericValue()
        for (xpr <- exprs) {
          xpr match {
            case e: ConstLiteralExpr => total = total + e.numeric
            case e: IdExpr => total = total + 1
            case e @ UnaryExpr(OpNegate(), _) => total = total - 1 // same type, just substract 1
            case e @ NAryExpr(OpTimes(), _) =>
              val c = multiplyConsts(e.terms.filter(isNumericConst)); total = total + c.numeric
            case e @ NAryExpr(OpPlus(), _) => throw new UnsupportedOperationException("Call mergeOperators first")
          }
        }
        if (total == 0) {
          ConstLiteralExpr(0)
        } else {
          // return expr based on the type
          val const = ConstLiteralExpr(total)
          exprs(0) match {
            case e: ConstLiteralExpr => const
            case e: IdExpr => if (total == 1) e else NAryExpr(OpTimes(), List(const, e))
            case e @ UnaryExpr(_, _) => const
            case e @ NAryExpr(OpTimes(), _) => {
              val nonConstPart = e.terms.filterNot(isNumericConst)
              if (total == 1)
                NAryExpr(OpTimes(), nonConstPart)
              else
                NAryExpr(OpTimes(), const :: nonConstPart)
            }
            case e @ NAryExpr(OpPlus(), _) => throw new UnsupportedOperationException("Call mergeOperators first")
          }
        }
      }

      override def leave(expr: NAryExpr): Expr = {
        expr.op match {
          case OpTimes() =>
            // multiply constants
            val (constPart, nonConstPart) = expr.terms.partition(isNumericConst)
            val constExpr = multiplyConsts(constPart)
            constExpr.value match {
              case 0 => constExpr
              // sort by lexicographic order
              case 1 => if (nonConstPart.isEmpty) constExpr else NAryExpr(OpTimes(), nonConstPart.sortBy(lexOrder))
              case _ => if (nonConstPart.isEmpty) constExpr
              else NAryExpr(OpTimes(), (constExpr :: nonConstPart).sortBy(lexOrder))
            }
          case OpPlus() =>
            // assume children are sorted in lexicographic order
            // note that the order may be lost due to grouping
            val tmap = expr.terms.groupBy(lexOrder)
            //	        for ((k,m) <- tmap) {
            //	          println(k + " " + m.map(PrettyPrinter.pretty(_)).mkString(", "))
            //	        }
            // try to add terms for each group
            val tlist = tmap.foldLeft(List.empty[Expr]) { case (acc, (k, xprs)) => simplifyAddition(xprs) :: acc }
            // remove terms with a constant zero
            val simplified = tlist.filterNot(xpr => xpr match {
              case e: ConstLiteralExpr => if (e.isNumeric) {
                e.numeric == 0
              } else {
                false
              }
              case _ => false
            })

            simplified.length match {
              case 0 => ConstLiteralExpr(0)
              case 1 => simplified(0)
              case _ => // finally sort by lexicographic order
                NAryExpr(OpPlus(), simplified.sortBy(lexOrder))
            }
        }
      }
    })

  // To be used for leaf nodes only
  def lexOrder(expr: Expr): String = {
    expr match {
      case e: ConstLiteralExpr => "zz~1"
      case e: IdExpr => e.pretty()
      case e: ArrayRefExpr => e.pretty()
      case e @ UnaryExpr(OpNegate(), _) => e.term.pretty()
      case e @ NAryExpr(OpTimes(), _)  =>
        val xprs = e.terms.filterNot(isNumericConst).sortBy(lexOrder)
        if (xprs.size == 1) xprs(0).pretty()
        else NAryExpr(OpPlus(), xprs).pretty()
      case e @ NAryExpr(OpPlus(), _)  =>
        val xprs = e.terms.sortBy(lexOrder)
        NAryExpr(OpPlus(), xprs).pretty()
      // required by ExpressionDenormalizationPass
      case e @ NAryExpr(OpMinus(), _)  =>
        val xprs = e.terms.sortBy(lexOrder)
        NAryExpr(OpMinus(), xprs).pretty()
      case e: FunctionCallExpr => e.pretty()
    }
  }
    
  // Add list of constants (with type promotion if necessary)  
  def addConsts(consts: List[Expr]): ConstLiteralExpr = {
    val init: NumericValue = 0
    val sum = consts.foldLeft(init) { case (acc, e) =>
      val c = e.asInstanceOf[ConstLiteralExpr]
      if (!c.isNumeric) throw new UnsupportedOperationException()
      acc + c.numeric
    }
    ConstLiteralExpr(sum)
  }
  // Multiply list of constants (with type promotion if necessary)
  def multiplyConsts(consts: List[Expr]): ConstLiteralExpr = {
    val init: NumericValue = 1
    val prod = consts.foldLeft(init) { case (acc, e) =>
      val c = e.asInstanceOf[ConstLiteralExpr]
      if (!c.isNumeric) throw new UnsupportedOperationException()
      acc * c.numeric
    }
    ConstLiteralExpr(prod)
  }
  
//  private def determineNumericKind(consts: List[Expr]): BasicType = {
//    var kind: BasicType = IntType()
//    for (e <- consts) {
//      val c = e.asInstanceOf[ConstLiteralExpr]
//      if (!c.isNumeric) throw new UnsupportedOperationException()
//      kind += c.kind
//    }
//    kind
//  }
  
  def isNumericConst(expr: Expr): Boolean = {
    expr match {
      case e: ConstLiteralExpr => e.isNumeric 
      case _ => false
    }
  }
  
  // TODO
  //def simplify(expr: Expr): Expr = localSimplify(expr)
  
  // TODO
  // add or multiply constant terms in N-ary expressions without expanding brackets
  def localSimplify(expr: Expr): Expr =
    ExpressionProcessor.process(expr, new ExpressionVisitor() {
      override def leave(expr: NAryExpr): Expr = {
        expr.op match {
          case OpPlus() => 
            var sum: NumericValue = 0
            var terms = expr.terms.reverse.foldLeft(List.empty[Expr]) { (acc, t) =>
              t match {
                case e: ConstLiteralExpr => if(e.isNumeric){
                  sum += e.numeric; acc
                } else {
                  t :: acc
                }
                case _ => t :: acc
              }
            }
            if (sum != 0) terms = terms :+ ConstLiteralExpr(sum)

            terms.size match {
              case 0 => ConstLiteralExpr(0)
              case 1 => terms(0)
              case _ => expr.update(terms = terms) 
            }
          case OpTimes() => 
            var prod: NumericValue = 0
            var terms = expr.terms.reverse.foldLeft(List.empty[Expr]) { (acc, t) =>
              t match {
                case e: ConstLiteralExpr => if(e.isNumeric){
                  prod *= e.numeric; acc
                } else {
                  t :: acc
                }
                case _ => t :: acc
              }
            }
            if (prod != 1) terms ::= ConstLiteralExpr(prod)

            terms.size match {
              case 0 => ConstLiteralExpr(0)
              case 1 => terms(0)
              case _ => if (prod == 0) ConstLiteralExpr(0) else expr.update(terms = terms) 
            }
          case _ => expr
        }
      }
      override def leave(expr: UnaryExpr): Expr = {
        expr.op match {
          case OpNegate() => 
            expr.term match {
              case e: ConstLiteralExpr => if(e.isNumeric){
                ConstLiteralExpr(-e.numeric)
              } else {
                expr
              }
              case _ => expr
            }
          case _ => expr
        }
      }
    })
  
  
  /*expr match {
    case e: NAryExpr => e
    case e: UnaryExpr        => e.update(term = mergeOperators(e.term))
    case e: ArrayRefExpr     => e
    case e: FieldRefExpr     => e
    case e: IdExpr           => e
    case e: ConstLiteralExpr => e
    case e: FunctionCallExpr => e
    case e: TypeIdExpr       => e
    case e: LinSpaceExpr     => e
    case e: LoopCountExpr    => e
    case e: OnesExpr         => e
    
    case e: IdExpr                   => e
    case e: ConstLiteralExpr[AnyVal] => e
    case e: OnesExpr                 => e
    case e: LoopCountExpr            => e
    case e: LinSpaceExpr             => e
    case e: ArrayRefExpr             => e
    case e @ UnaryExpr(op, term)     => e.copy(op, term = mergeOperators(term))
    case e @ NAryExpr(op1, terms1) =>
      var ts = terms1.reverse.foldLeft(List.empty[Expr])((acc, xpr) => mergeOperators(xpr) match {
        // merge if same operator, or single terms
        case e @ NAryExpr(op2, terms2) => if ((op1 == op2) || (terms2.size == 1)) terms2 ::: acc else e :: acc
        case other                     => other :: acc
      })
      e.copy(op1, terms = ts)
  }*/
}

/*package   core

import   model._

object Algebra {
  
  // merge similar children operators with parent
  def mergeOperators(expr: Expr): Expr = expr match {
    case e: IdExpr                   => e
    case e: ConstLiteralExpr[AnyVal] => e
    case e: OnesExpr                 => e
    case e: LoopCountExpr            => e
    case e: LinSpaceExpr             => e
    case e: ArrayRefExpr             => e
    case e @ UnaryExpr(op, term)     => e.copy(op, term = mergeOperators(term))
    case e @ NAryExpr(op1, terms1) =>
      var ts = terms1.reverse.foldLeft(List.empty[Expr])((acc, xpr) => mergeOperators(xpr) match {
        // merge if same operator, or single terms
        case e @ NAryExpr(op2, terms2) => if ((op1 == op2) || (terms2.size == 1)) terms2 ::: acc else e :: acc
        case other                     => other :: acc
      })
      e.copy(op1, terms = ts)
  }


}*/