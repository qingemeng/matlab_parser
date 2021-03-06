package matlabParser


/**
 * Created with IntelliJ IDEA.
 * User: gemengqin
 * Date: 12/30/13
 * Time: 10:18 PM
 * To change this template use File | Settings | File Templates.
 */

import scala.language.postfixOps
import scala.collection.mutable._
import model.OpPostfixInc
import model.OpLogicalAnd
import model.OpEquals
import model.OpMinus
import model.OpTimes
import model.OpGreaterEq
import model.OpLessThan
import model.OpPlus
import model.OpPostfixDec
import model.OpNotEquals
import model.OpLogicalOr
import model.OpAssign
import model.OpPow
import model.OpLessEq
import model.OpPrefixInc
import model.OpDotProd
import model.OpPrefixDec
import model.OpDivide
import model.OpGreaterThan
import scala.util.parsing.input.Positional
import scala.collection.mutable

//import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator._

import model._
import model.expression._
import model.statement._
import model.property.HasProperties
import refactoring.matlab.model._
import model.OpTimesAssign
import model.OpPostfixInc
import model.OpLogicalAnd
import model.OpEquals
import model.OpTimesAssign
import model.OpPostfixInc
import model.OpLogicalAnd
import model.OpEquals
import model.OpMinus
import model.OpTimes
import model.OpGreaterEq
import model.OpLessThan
import model.OpPlus
import model.OpMinusAssign
import model.OpPostfixDec
import model.OpNotEquals
import model.OpLogicalOr
import model.OpAssign
import model.OpPow
import model.OpLessEq
import model.OpPlusAssign
import model.OpDivideAssign
import model.OpPrefixInc
import model.OpDotProd
import model.OpPrefixDec
import model.OpDivide
import model.OpGreaterThan

//import model.OpMatProd
import model.OpMinus
import model.OpTimes
import model.OpPermute
//import model.OpOuterProd
import model.OpGreaterEq
import model.OpLessThan
import model.OpPlus
import model.OpMinusAssign
import model.OpPostfixDec
import model.OpNotEquals
import model.OpLogicalOr
import model.OpAssign
import model.OpPow
import model.OpLessEq
import model.OpPlusAssign
import model.OpDivideAssign
import model.OpProd
import model.OpSum
import model.OpPrefixInc
import model.OpDotProd
import model.OpPrefixDec
import model.OpDivide
import model.OpGreaterThan
import model.OpMatPow

object MatlabParser extends JavaTokenParsers with PackratParsers {
  val declMap = HashMap[IdName, BasicType]()
//  val funcNames  = mutable.MutableList.empty[IdExpr]


  //lazy val keywords: PackratParser[Any] = "var" | "for"
//  lazy val eol: Parser[Any] = rep(""";(\r?\n)?+""".r)
    lazy val eol: Parser[Any] = rep(""";(\r?\n)?+""".r)


  lazy val source: PackratParser[StatementBlock] =
   positioned(
      (statement*) ^^ {
        case stmts => StatementBlock(stmts)
      }
    )

  lazy val functions: PackratParser[List[FunctionDefStatement]] = rep(function_def)

  def newFunction(name: IdName, paramsIn: List[Parameter],paramsOut: List[Parameter], body: Statement): FunctionDefStatement = {
    val inParams = ListBuffer[IdName]()
    val outParams = ListBuffer[IdName]()
    val params = ListBuffer[IdName]()
    if(paramsIn!=null&&paramsOut!=null){
    paramsIn.foreach(param => {
      params += param.idName
      inParams += param.idName
      params += param.idName
    })
    paramsOut.foreach(param =>{
      outParams +=param.idName;
    })
    }

    FunctionDef(name, params.toList, inParams.toList, outParams.toList, body)
    FunctionDefStatement(FunctionDef(name, params.toList, inParams.toList, outParams.toList, body))

  }



  lazy val function_def: PackratParser[FunctionDefStatement] =   positioned(
    "function" ~> ("["~>parameters<~"]")  ~ ( "=" ~>identifierName) ~ ("("~>(parameters)<~")") ~blockStmt <~"end"^^{
      case out~name~in~body => newFunction(name,in, out, body)
    } |
      "function" ~> identifierName ~("(" ~> (parameters)<~")") ~blockStmt <~"end"^^{
        case name~in~body => newFunction(name, in, null,body)
      } |
      "function" ~> ("["~>parameters<~"]")~ ("=" ~>identifierName)~blockStmt <~"end"^^{
        case out~name~body => newFunction(name,null, out,body)
      }  |
      "function" ~> identifierName ~ blockStmt<~"end"^^{
        case name~body => newFunction(name,null,null,body)
      }
  )

  class Parameter(val idName: IdName) extends Positional
  case class InParameter(name:IdName) extends Parameter(name)
  case class OutParameter(name:IdName) extends Parameter(name)

  lazy val testExpr: PackratParser[String] =( (testExpr <~ "+") ~ testTerm) ^^ {case a~b => a.toString + " + " + b.toString }
////
lazy val testExpr2: PackratParser[String] = testTerm ~ y ^^{case t~ye => " testTerm: " + t.toString +" y : "+ ye.toString }
   lazy val testTerm :PackratParser[String] = "t"|"f"
   lazy val y: PackratParser[String] = "+" ~>testTerm ~ y ^^{case t~ye => " testTerm: " + t.toString +" y : "+ ye.toString } |
     eol ^^ { case end => end.toString}

  lazy val parameters: PackratParser[List[Parameter]] =repsep(parameter, ",")

  lazy val parameter: PackratParser[Parameter] =   positioned(
    identifierName  ^^ { case name => InParameter(name)} |
      identifierName ^^ { case name => OutParameter(name)}
  )

  /******************************************************************************************/

  lazy val statement: PackratParser[Statement] = positioned(
      statementCmd
  )
//  lazy val statement: PackratParser[Statement] = positioned(
//      statementCmd)

  lazy val statementCmd: PackratParser[Statement] = positioned (
    forStmt|
  whileStmt|
   switchStmt|
    ifStmt|
      singleLineStmt|
  function_def
  )


  lazy val blockStmt : PackratParser[StatementBlock] =  positioned(
    (statement*) ^^ { case stmts => StatementBlock(stmts)}
  )



  lazy val singleLineStmt: PackratParser[Statement] = positioned(
    globalDeclarationStmt <~ eol |
      assignmentStmt <~ eol |
      caseStmt <~ eol |
      retStmt <~ eol |
      defaultStmt <~ eol |
      breakStmt <~ eol |
      continueStmt <~ eol|
//      simpleStmt<~eol |
      functionCallStatement<~eol
  )

  //TODO:fix the single line evaluation
//  lazy val evalStmt: PackratParser[Statement] =


  lazy val simpleStmt:PackratParser[Statement]=
      cellExpr^^{case cellExpr=>ExpressionStatement(cellExpr)}  //|
//      matrixExpr^^{case matrixExpr => ExpressionStatement(matrixExpr)}
//      const_literal^^ {case constExpr=>ExpressionStatement(constExpr)}  |
////      identifier^^{case id=>ExpressionStatement(id)}
//  simpleFactor^^{case simpleFactor => ExpressionStatement(simpleFactor)}
//  cellExpr|matrixExpr|simpleFactor

  //@TODO:declare global var
  lazy val globalDeclarationStmt: PackratParser[Statement] =   positioned(
    "global"~> declarator ^^{case declarator=> GlobalDeclarationStatement("global",List(declarator))}
  )
//  assignmentStmt|
//  "global"~> declarator ^^{case declarator=> DeclarationStatement("",List(declarator))}|
//  "local"~> declarator ^^{case declarator => DeclarationStatement(List(declarator)) }
  lazy val declarator: PackratParser[Declarator] =  positioned(
  identifierName^^{case idname => Declarator(idname)}
)

  lazy val assignmentLHSexpr: PackratParser[Expr] =  positioned(
    arrayRefExpr |
      identifier
  )

  lazy val assignmentStmt: PackratParser[Statement] =  positioned(
//  opt(",")~>   assignmentLHSexpr ~ ("+=" ~> expr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpPlusAssign())} |
    opt(",")~>   assignmentLHSexpr ~ ("=" ~> expr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpAssign())}|
      rangeAssignStmt
//      arrayAssignStmt

//  opt(",")~>   assignmentLHSexpr ~ ("-=" ~> expr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpMinusAssign())} |
//  opt(",")~>   assignmentLHSexpr ~ ("*=" ~> expr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpTimesAssign())} |
//  opt(",")~>   assignmentLHSexpr ~ ("/=" ~> expr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpDivideAssign())}
  )

  lazy val forStmt: PackratParser[Statement] = positioned(
    ("for"~> rangeAssignStmt) ~blockStmt<~"end"^^{
      case range~body =>ForStatement(range,null,null, body)
    }|
      ("for"~> assignmentStmt) ~blockStmt<~"end"^^{
        case init~body =>ForStatement(init,null,null, body)
      }
  )



  lazy val ifStmt: PackratParser[Statement] =   positioned(
//    ("if" ~> opt("(")~>expr<~opt(")"))~ blockStmt <~"end"^^ { case cond~stmt => IfStatement(cond, stmt,null, null)}|
    (("if" ~> opt("(")~>expr<~opt(")") ) ~blockStmt )~ opt(elseifStmt*)~opt(("else" ~> blockStmt)) <~"end"^^ { case cond~thebody~elseifStmts~elsebody => IfStatement(cond, thebody,elseifStmts, elsebody)}
//  (("if" ~> opt("(")~>expr<~opt(")") )) ~ blockStmt ~ opt(elseifBlocks)<~"end"^^ { case cond~thebody~elseifStmts => IfStatement(cond, thebody,elseifStmts,null)}|
//      ( ("if" ~> opt("(")~>expr<~opt(")") )) ~ blockStmt ~ ("else" ~> opt(blockStmt)) <~"end"^^ { case cond~thebody~elsebody => IfStatement(cond, thebody,null, elsebody)}|
//      ( ("if" ~> opt("(")~>expr<~opt(")")) )~ blockStmt <~"end"^^ { case cond~stmt => IfStatement(cond, stmt)}
  )
  //Todo:elseif

//  lazy val elseifBlocks: PackratParser[StatementBlock]=
//  (("elseif" ~> blockStmt)*) ^^ {case blockStmts => ElseifStatements(blockStmts)}
  lazy val elseifStmt : PackratParser[ElseifStatement] = positioned(
  ("elseif"~>opt("(")~>expr<~opt(")")) ~blockStmt ^^{case cond~body => ElseifStatement(cond,body)}
)



  lazy val whileStmt: PackratParser[Statement] =  positioned(
    ("while"  ~>opt("(")~> expr<~opt(")")) ~ blockStmt <~"end"^^ { case cond~stmt => WhileStatement(cond, stmt)}
  )

  //  lazy val doStmt: PackratParser[Statement] =
  //    ("do" ~> statement) ~  ("while" ~> "(" ~> expr <~ ")") ^^ { case stmt~cond => DoStatement(cond, stmt)}

  lazy val switchStmt: PackratParser[Statement] =  positioned(
    ("switch" ~> expr ) ~ blockStmt<~"end" ^^ { case cond~stmt => SwitchStatement(cond, stmt)}
  )

  lazy val breakStmt: PackratParser[Statement] =  positioned(
    "break" ^^ {x => BreakStatement()}
  )

  lazy val continueStmt: PackratParser[Statement] =  positioned(
    "continue" ^^ {x => ContinueStatement()}
  )

  lazy val defaultStmt: PackratParser[Statement] =  positioned(
    "otherwise" ^^ {x => DefaultStatement()}
  )

  lazy val caseStmt: PackratParser[Statement] =  positioned(
    "case" ~> expr  ^^ {xpr => CaseStatement(xpr)}
  )

  lazy val retStmt: PackratParser[Statement] =  positioned(
    "return" ~> expr  ^^ {xpr => ReturnStatement(xpr)} |
      "return"  ^^ {x => ReturnStatement()}
  )

  lazy val rangeAssignStmt: PackratParser[Statement] =  positioned(
    assignmentLHSexpr ~ ("=" ~> sliceExpr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpAssign())}
  )
//  lazy val arrayAssignStmt: PackratParser[Statement] =
//     assignmentLHSexpr ~ ("=" ~> arrayExpr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpAssign())}

  /******************************************************************************************/

  /******************************************************************************************/
  lazy val expr: PackratParser[Expr] = positioned( arrayExpr|cellExpr) //|matrixExpr


  //Array Expressions
//
//  lazy val arrayExpr: PackratParser[Expr] =
//    "["~opt(",")~"]" ^^{case s =>ArrayCompositionExpr()}| //empty
//      "["~>eleListExpr<~"]"^^{case arrayFactors =>ArrayCompositionExpr(arrayFactors)}|
//      //sliceExpr^^{case sliceExpr =>ArrayCompositionExpr(sliceToElelist(sliceExpr))}|  // row vector
//      "["~>repsep(eleListExpr,";")<~"]"^^{case arrayExprs =>ArrayCompositionExpr(
//       arrayExprs.map(each => ArrayCompositionExpr(each))
//      )}//|   // column vector
////    arrayConditionalExpr

  lazy val arrayExpr: PackratParser[Expr] =positioned(arrayBinaryExpr|arrayUnitExpr)
  lazy val eleListExpr: PackratParser[List[Expr]] =
    repsep(arrayFactor,",")




  //  lazy val oneDArrExpr: PackratParser[Expr] =
//    repsep(arrayFactor,",") ^^{case arrayFactors =>ArrayCompositionExpr(arrayFactors)}


//      lazy val arrayConditionalExpr: PackratParser[Expr] =
//    (arrayConditionalExpr <~ "&&") ~ arrayCompareExpr ^^ { case lhs~rhs => NAryExpr(OpLogicalAnd(), List(lhs, rhs)) } |
//      (arrayConditionalExpr <~ "||") ~ arrayCompareExpr ^^ { case lhs~rhs => NAryExpr(OpLogicalOr(), List(lhs, rhs)) } |
//      arrayCompareExpr
  lazy val arrayBinaryExpr = positioned(arrayConditionalExpr  )
  lazy val arrayConditionalExpr: PackratParser[Expr] =   positioned(
    (arrayConditionalExpr <~ "&") ~ arrayCompareExpr ^^ { case lhs~rhs => NAryExpr(EleWiseOpLogicalAnd(), List(lhs, rhs)) } |
      (arrayConditionalExpr <~ "|") ~ arrayCompareExpr ^^ { case lhs~rhs => NAryExpr(EleWiseOpLogicalOr(), List(lhs, rhs)) } |
      arrayCompareExpr
  )

  lazy val arrayCompareExpr: PackratParser[Expr] =   positioned(
    (arrayTerm <~ "==") ~ arrayTerm ^^ { case lhs~rhs => NAryExpr(OpEquals(), List(lhs, rhs)) } |
      (arrayTerm <~ "~=") ~ arrayTerm ^^ { case lhs~rhs => NAryExpr(OpNotEquals(), List(lhs, rhs)) } |
      (arrayTerm <~ "<") ~ arrayTerm ^^ { case lhs~rhs => NAryExpr(OpLessThan(), List(lhs, rhs)) } |
      (arrayTerm <~ "<=") ~ arrayTerm ^^ { case lhs~rhs => NAryExpr(OpLessEq(), List(lhs, rhs)) } |
      (arrayTerm <~ ">") ~ arrayTerm ^^ { case lhs~rhs => NAryExpr(OpGreaterThan(), List(lhs, rhs)) } |
      (arrayTerm <~ ">=") ~ arrayTerm ^^ { case lhs~rhs => NAryExpr(OpGreaterEq(), List(lhs, rhs)) } |
      arrayTerm
  )

  lazy val arrayTerm: PackratParser[Expr] = positioned(
    (arrayTerm <~ "+") ~ arrayTerm2 ^^ { case lhs~rhs => NAryExpr(OpPlus(), List(lhs, rhs)) } |
      (arrayTerm <~ "-") ~ arrayTerm2 ^^ { case lhs~rhs => NAryExpr(OpMinus(), List(lhs, rhs)) } |
      arrayTerm2
  )

  lazy val arrayTerm2: PackratParser[Expr] = positioned(
    (arrayTerm2 <~ "*") ~ arrayUnaryTerm ^^ { case lhs~rhs => NAryExpr(OpTimes(), List(lhs, rhs)) } |
      (arrayTerm2 <~ "/") ~ arrayUnaryTerm ^^ { case lhs~rhs => NAryExpr(OpDivide(), List(lhs, rhs)) } |
      (arrayTerm2<~ "\\") ~ arrayUnaryTerm ^^ { case lhs~rhs => NAryExpr(OpLeftDivide(),List(lhs,rhs))}|
      (arrayTerm2 <~ "^") ~ arrayUnaryTerm ^^ { case lhs~rhs => NAryExpr(OpPow(), List(lhs, rhs)) } |
      (arrayTerm2 <~ ".*") ~ arrayUnaryTerm ^^ { case lhs~rhs => NAryExpr(OpDotProd(), List(lhs, rhs)) } |
//      (arrayTerm2 <~ "<o>") ~ arrayUnaryTerm ^^ { case lhs~rhs => NAryExpr(OpOuterProd(), List(lhs, rhs)) } |
//      (arrayTerm2 <~ "**") ~ arrayUnaryTerm ^^ { case lhs~rhs => NAryExpr(OpMatProd(), List(lhs, rhs)) } |
//      (arrayTerm2 <~ "*^") ~ arrayUnaryTerm ^^ { case lhs~rhs => NAryExpr(OpMatPow(), List(lhs, rhs)) } |
      arrayUnaryTerm
  )

  lazy val arrayUnaryTerm: PackratParser[Expr] =  positioned(
//    sumTerm | prodTerm | permuteTerm |
//      identifier ~ ("(" ~> repsep(arrayTerm, ",") <~ ")") ^^ { case name~params => FunctionCallExpr(name,params)} |
//      "(" ~> arrayExpr <~ ")" |
    "+" ~> arrayFactor ^^ { case x => UnaryExpr(OpUnaryPlus(), x)} |
      "-" ~> arrayFactor ^^ { case x => UnaryExpr(OpUnaryMinus(), x)} |
//      arrayRefExpr|
      arrayFactor

  )

  lazy val functionCallStatement: PackratParser[Statement] =positioned(
    identifier ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ { case name~params =>
//      if()
      FunctionCallStatement(FunctionCallExpr(name,params))
    }

  )
  lazy val functionCallExpr: PackratParser[FunctionCallExpr] = positioned(
    identifier ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ {
      case name~params =>FunctionCallExpr(name,params)
    }
  )

  //      "-" ~> arrayTerm ^^ { case x => UnaryExpr(OpNegate(), x) }

//  lazy val sumTerm: PackratParser[Expr] =
//    ("Sum(" ~> arrayTerm <~ ",") ~ integerList <~ ")" ^^ { case xpr~list => UnaryExpr(OpSum(list), xpr) } |
//      "Sum(" ~> arrayTerm <~ ")" ^^ { case xpr => UnaryExpr(OpSum(List.empty[Int]), xpr) }
//
//  lazy val prodTerm: PackratParser[Expr] =
//    ("Prod(" ~> arrayTerm <~ ",") ~ integerList <~ ")" ^^ { case xpr~list => UnaryExpr(OpProd(list), xpr) } |
//      "Prod(" ~> arrayTerm <~ ")" ^^ { case xpr => UnaryExpr(OpProd(List.empty[Int]), xpr) }
//
//  lazy val permuteTerm: PackratParser[Expr] =
//    ("Permute(" ~> arrayTerm <~ ",") ~ integerList <~ ")" ^^ { case xpr~list => UnaryExpr(OpPermute(list), xpr) }

//  lazy val sizeOf: PackratParser[Expr]=
//    ("size("~> arrayExpr)

  //TODO: Gm,Symbol reference and builtin func



  lazy val arrayFactor : PackratParser[Expr] =  positioned(
      functionCallExpr|
      arrayRefExpr |
        sliceExpr|
      simpleFactor |
      cellUnitExpr|
   arrayUnitExpr
  )

  lazy val arrayUnitExpr: PackratParser[Expr] =  positioned(
    "["~opt(",")~"]" ^^{case s =>ArrayCompositionExpr()}| //empty
      "["~>eleListExpr<~"]"^^{case arrayFactors =>ArrayCompositionExpr(arrayFactors)}|
      //sliceExpr^^{case sliceExpr =>ArrayCompositionExpr(sliceToElelist(sliceExpr))}|  // row vector
      "["~>repsep(eleListExpr,";")<~"]"^^{case arrayExprs =>ArrayCompositionExpr(
        arrayExprs.map(each => ArrayCompositionExpr(each))
      )}//|   // column vector
  //    arrayConditionalExpr
  )

  lazy val cellUnitExpr: PackratParser[Expr] =   positioned(
    "{"~opt(",")~"}" ^^{case s =>CellCompositionExpr()}| //empty
      "{"~>eleListExpr<~"}"^^{case cellFactors =>CellCompositionExpr(cellFactors)}|
      //sliceExpr^^{case sliceExpr =>ArrayCompositionExpr(sliceToElelist(sliceExpr))}|  // row vector
      "{"~>repsep(eleListExpr,";")<~"}"^^{case cellExprs =>CellCompositionExpr(
        cellExprs.map(each => CellCompositionExpr(each))
      )}
  )

//  lazy val arrayExprFactor: PackratParser[Expr] =
//  "["~opt(",")~"]" ^^{case s =>UnaryExpr(OpMakeFactor(), ArrayCompositionExpr())}| //empty
//    "["~>eleListExpr<~"]"^^{case arrayFactors =>UnaryExpr(OpMakeFactor(),ArrayCompositionExpr(arrayFactors))}|
//    //sliceExpr^^{case sliceExpr =>ArrayCompositionExpr(sliceToElelist(sliceExpr))}|  // row vector
//    "["~>repsep(eleListExpr,";")<~"]"^^{case arrayExprs =>ArrayCompositionExpr(
//      arrayExprs.map(each => UnaryExpr(OpMakeFactor(),ArrayCompositionExpr(each)))
//    )}|   // column vector
//    arrayConditionalExpr

//  matrixExpr


//  lazy val arrayList: PackratParser[Expr] =
//    repsep(oneDArrExpr,";") ^^{case arrayList =>ArrayCompositionExpr(arrayList)  }


  /******************************************************************************************/
  // Array Reference

  lazy val arrayRefExpr: PackratParser[ArrayRefExpr] =  positioned(
    identifier ~ rep1("(" ~> sliceExpr <~ ")") ^^ { case id~slices => ArrayRefExpr(id, slices)}  |
      identifier ~ ("(" ~> repsep(sliceExpr,"," )<~ ")" )^^ { case id~slices => ArrayRefExpr(id, slices)}
  )


  lazy val sliceExpr: PackratParser[Expr] =   positioned(
    simpleExpr ~ (":" ~> simpleExpr) ~ (":" ~> simpleExpr) ^^ { case lower~upper~stride => SliceExpr(lower, upper, stride)} |
      simpleExpr ~ (":" ~> simpleExpr) ^^ { case lower~upper => SliceExpr(lower, upper)} |
  ":" ^^ { x => SliceExpr()} |
      simpleExpr
  )

  /******************************************************************************************/
  // Simple Expressions
  lazy val simpleExprList: PackratParser[Expr] =   positioned(
    repsep(simpleExpr, ",") ^^ { case exprs => exprs.size match {
      case 0 => throw new UnsupportedOperationException("ExpressionList of size 0")
      case 1 => exprs(0)
      case _ => ExpressionListExpr(exprs)
    } } |
      simpleExpr
  )

  lazy val simpleExpr: PackratParser[Expr] =   positioned(
    simpleConditionalExpr
  )

lazy val simpleConditionalExpr: PackratParser[Expr] =  positioned(
    (simpleConditionalExpr <~ "&&") ~ simpleCompareExpr ^^ { case lhs~rhs => NAryExpr(OpLogicalAnd(), List(lhs, rhs)) } |
      (simpleConditionalExpr <~ "||") ~ simpleCompareExpr ^^ { case lhs~rhs => NAryExpr(OpLogicalOr(), List(lhs, rhs)) } |
      simpleCompareExpr
  )

  lazy val simpleCompareExpr: PackratParser[Expr] =  positioned(
    (simpleArithmeticExpr <~ "==") ~ simpleArithmeticExpr ^^ { case lhs~rhs => NAryExpr(OpEquals(), List(lhs, rhs)) } |
      (simpleArithmeticExpr <~ "~=") ~ simpleArithmeticExpr ^^ { case lhs~rhs => NAryExpr(OpNotEquals(), List(lhs, rhs)) } |
      (simpleArithmeticExpr <~ "<") ~ simpleArithmeticExpr ^^ { case lhs~rhs => NAryExpr(OpLessThan(), List(lhs, rhs)) } |
      (simpleArithmeticExpr <~ "<=") ~ simpleArithmeticExpr ^^ { case lhs~rhs => NAryExpr(OpLessEq(), List(lhs, rhs)) } |
      (simpleArithmeticExpr <~ ">") ~ simpleArithmeticExpr ^^ { case lhs~rhs => NAryExpr(OpGreaterThan(), List(lhs, rhs)) } |
      (simpleArithmeticExpr <~ ">=") ~ simpleArithmeticExpr ^^ { case lhs~rhs => NAryExpr(OpGreaterEq(), List(lhs, rhs)) } |
      simpleArithmeticExpr
  )

  lazy val simpleArithmeticExpr: PackratParser[Expr] =   positioned(
    (simpleArithmeticExpr <~ "+") ~ simpleTerm ^^ { case lhs~rhs => NAryExpr(OpPlus(), List(lhs, rhs)) } |
      (simpleArithmeticExpr <~ "-") ~ simpleTerm ^^ { case lhs~rhs => NAryExpr(OpMinus(), List(lhs, rhs)) } |
      simpleTerm
  )

  lazy val simpleTerm: PackratParser[Expr] =   positioned(
    (simpleTerm <~ "*") ~ simpleUnaryTerm ^^ { case lhs~rhs => NAryExpr(OpTimes(), List(lhs, rhs)) } |
      (simpleTerm <~ "/") ~ simpleUnaryTerm ^^ { case lhs~rhs => NAryExpr(OpDivide(), List(lhs, rhs)) } |
      (simpleTerm <~ "\\") ~ simpleUnaryTerm ^^ { case lhs~rhs => NAryExpr(OpLeftDivide(), List(lhs, rhs)) } |
      simpleUnaryTerm
  )

  lazy val simpleUnaryTerm: PackratParser[Expr] =    positioned(
    "+" ~> simpleFactor ^^ { case x => UnaryExpr(OpUnaryPlus(), x)} |
      "-" ~> simpleFactor ^^ { case x => UnaryExpr(OpUnaryMinus(), x)} |
//      simpleFactor <~ "++" ^^ { case x => UnaryExpr(OpPostfixInc(), x)} |
//      simpleFactor <~ "--" ^^ { case x => UnaryExpr(OpPostfixDec(), x)} |
      "(" ~> simpleExpr <~ ")" |
      simpleFactor
  )

  lazy val simpleFactor : PackratParser[Expr] =  positioned(
    const_literal |
      identifier
  )

  /******************************************************************************************/
  lazy val const_literal: PackratParser[ConstLiteralExpr] =   positioned(
    stringLiteral ^^ {x => ConstLiteralExpr(x)} |
      floatingPointNumber ^^ {x => toFloat(x)} |
      decimalNumber ^^ {x => toFloat(x)} |
      wholeNumber ^^ {x => ConstLiteralExpr(x.toInt)} |
      booleanLiteral
  )
  override def stringLiteral: Parser[String] =
  ("\'"+"""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""+"\'").r

  def toFloat(x: String) = {
    try {
      ConstLiteralExpr(x.toInt)
    } catch {
      case e: NumberFormatException => ConstLiteralExpr(x.toFloat)
    }
  }
//  def toFactor(x:Expr): Expr ={
//
//  }

//  def sliceToElelist(sliceExpr:SliceExpr) :List[Expr]={
//    val list = List()
//    var lower = sliceExpr.lowerBound.asInstanceOf[ConstLiteralExpr].value.asInstanceOf[Int]
//    val upper = sliceExpr.upperBound.asInstanceOf[ConstLiteralExpr].value.asInstanceOf[Int]
//    val stride = sliceExpr.stride.asInstanceOf[ConstLiteralExpr].value.asInstanceOf[Int]
//    //just int
//    while(lower != upper){
//      list.:+( ConstLiteralExpr(lower.toString.toInt))
//      lower = lower + stride
//    }
//    list
//
//
//  }

  //[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?

  lazy val booleanLiteral: PackratParser[ConstLiteralExpr] =  positioned(
    ("true" | "false") ^^ {x => ConstLiteralExpr(x.toBoolean)}
  )

//  lazy val integerList: PackratParser[List[Int]] =  "[" ~> repsep(integer, ",") <~ "]"
  lazy val integer: PackratParser[Int] = wholeNumber ^^ {x => x.toInt}

  lazy val identifier: PackratParser[IdExpr] = positioned( identifierName ^^ {x => IdExpr(x)}   )

  lazy val identifierName: PackratParser[IdName] =  positioned("[a-zA-Z][a-zA-Z0-9_]*".r ^^ {x => IdName(x)})
  //lazy val identifierName: PackratParser[IdName] = not(keywords)~ident ^^ { case x~name => IdName(name)}

  //override val whiteSpace = """(\s|(//.*\n))+""".r

  //Ignores C-style comments
  //override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
  override val whiteSpace =  """(\s|%.*|%\{(\n|\r)+\}%)+""".r


  /******************************************************************************************/
  lazy val cellExpr: PackratParser[Expr] =  cellUnitExpr|cellBinaryExpr
  lazy val cellBinaryExpr: PackratParser[Expr] = arrayBinaryExpr
//  "{"~opt(",")~"}" ^^{case s =>ArrayCompositionExpr()}|
//      "{"~>repsep(arrayFactor,",")<~"}"^^{case arrayFactors =>CellCompositionExpr(arrayFactors)} //|
//      "{"~>arrayList<~"}"
//    "{"~opt(separator)~"}" ^^{case s =>ArrayCompositionExpr()}|
//      "{"~>repsep(arrayFactor,separator)<~"}"^^{case arrayFactors =>CellCompositionExpr(arrayFactors)} |
//      "{"~>arrayList<~"}"
//

//  lazy val separator = ","|" "

//  lazy val matrixExpr :  PackratParser[Expr] =
//    "["~opt(",")~"]" ^^{case s =>ArrayCompositionExpr()}|
//      "["~>repsep(arrayFactor,",")<~"]"^^{case arrayFactors =>MatrixCompositionExpr(arrayFactors)} |
//      "["~>arrayList<~"]"|
//      "["~>repsep(sliceExpr,",")<~"]"^^{case s =>MatrixCompositionExpr(s)}


//  lazy val matrixAccessExpr: PackratParser[Expr]=
//  identifier ~("("~>repsep(sliceExpr, ",")<~")" )^^{case arr~slices => FunctionCallExpr(arr,slices)}

  /******************************************************************************************/


  def parseExpr(text : String){
    declMap.clear()
    parseAll(expr, text) match {
      case Success (expr, _) => Left(declMap.toMap, expr)
      case f => Right(f.toString)
    }
  }

  def parseStatement(text: String) = {
    declMap.clear()
    parseAll(statement, text)  match {
      case Success (statement, _) => Left(declMap.toMap, statement)
      case f => Right(f.toString)
    }
  }

  def parseSource(text: String) = {
    declMap.clear()
    parseAll(source, text)  match {
      case Success (statement, _) =>{
        Left(declMap.toMap, statement
        )
//        val pos = statement.pos
//        print("pos = " +pos)
      }
      case f => Right(f.toString)
    }
  }

  def parseFunctions(text: String) = {
    declMap.clear()
    parseAll(functions, text)  match {
      case Success (funcs, _) => Left(declMap.toMap, funcs)
      case f => Right(f.toString)
    }
  }
  def parseTest(testTxt: String) = {
  declMap.clear()
  parseAll(simpleArithmeticExpr, testTxt)  match {
          case Success (test, _) => Left(declMap.toMap, test)
          case f => Right(f.toString)
        }
}
//
}
