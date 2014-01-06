package matlabParser


/**
 * Created with IntelliJ IDEA.
 * User: gemengqin
 * Date: 12/30/13
 * Time: 10:18 PM
 * To change this template use File | Settings | File Templates.
 */

import scala.language.postfixOps
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator._

import model._
import model.expression._
import model.statement._
import model.property.HasProperties
import refactoring.matlab.model.FunctionDefStatement

object MatlabParser extends JavaTokenParsers with PackratParsers {
  val declMap = HashMap[IdName, BasicType]()

  //lazy val keywords: PackratParser[Any] = "var" | "for"
//  lazy val eol: Parser[Any] = rep(""";(\r?\n)?+""".r)
    lazy val eol: Parser[Any] = rep(""";(\r?\n)?+""".r)


  lazy val source: PackratParser[StatementBlock] =
    (statement*) ^^ { case stmts => StatementBlock(stmts)}
  //  lazy val source :PackratParser[StatementList] =
  //      (statement*) ^^ { case stmts => StatementBlock(stmts)}

  //  lazy val StatementList: PackratParser[]

  lazy val functions: PackratParser[List[FunctionDefStatement]] = rep(function_def)

  //  def newFunction(name: IdName, paramsList: List[Parameter], body: Statement, typ: Option[BasicType]): FunctionDef = {
  //    val inParams = ListBuffer[IdName]()
  //    val outParams = ListBuffer[IdName]()
  //    val params = ListBuffer[IdName]()
  //    paramsList.foreach(param => {
  //      declMap += param.idName -> param.typ
  //      params += param.idName
  //      param match {
  //        case p: InParameter => inParams += p.idName
  //        case p: OutParameter => outParams += p.idName
  //        case p: InOutParameter => {
  //          inParams += p.idName
  //          outParams += p.idName
  //        }
  //      }
  //    })
  //
  //    FunctionDef(name, params.toList, inParams.toList, outParams.toList, body, typ)
  //  }
  def newFunction(name: IdName, paramsIn: List[Parameter],paramsOut: List[Parameter], body: Statement): FunctionDefStatement = {
    val inParams = ListBuffer[IdName]()
    val outParams = ListBuffer[IdName]()
    val params = ListBuffer[IdName]()
    if(paramsIn!=null&&paramsOut!=null){
    paramsIn.foreach(param => {
      //    declMap += param.idName -> param.typ
      params += param.idName
      //    param match {
      //      InParameter => inParams += p.idName
      //      case p: InOutParameter => {
      inParams += param.idName
      params += param.idName

      //        outParams += p.idName
      //      }
      //    }
    })
    paramsOut.foreach(param =>{
      outParams +=param.idName;
    })
    }


    //  FunctionDef(name, params.toList, inParams.toList, outParams.toList, body, typ)
    FunctionDef(name, params.toList, inParams.toList, outParams.toList, body)
    FunctionDefStatement(FunctionDef(name, params.toList, inParams.toList, outParams.toList, body))

  }

  //  lazy val function: PackratParser[FunctionDef] =
  //    "def" ~> identifierName ~ ("(" ~> parameters <~ ")") ~ (":" ~> declarationType) ~ blockStmt ^^ {
  //      case name~params~typ~body => newFunction(name, params, body, Some(typ))
  //    } |
  //      "def" ~> identifierName ~ ("(" ~> parameters <~ ")") ~ blockStmt ^^ {
  //        case name~params~body => newFunction(name, params, body, None)
  //      }
  ////
  //  function_definition
  //    : FUNCTION function_return? ID parameter_list? nloc
  //  func_or_statement_list
  //  END -> ^(FUNCTION ID function_return parameter_list func_or_statement_list)
  //  ;
  //
  //  function_return
  //    : ID EQ -> ^(FUNCTION_RETURN ID)
  //  | LSBRACE (options {greedy=false;} : ID COMMA?)+ RSBRACE EQ -> ^(FUNCTION_RETURN ID+)
  //  ;

  lazy val function_def: PackratParser[FunctionDefStatement] =
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
  //
  //  lazy val function_return: PackratParser[List[Parameter]]=


  //  class Parameter(val idName: IdName, val typ: BasicType)
  //  case class InParameter(name: IdName, t: BasicType) extends Parameter(name, t)
  //  case class OutParameter(name: IdName, t: BasicType) extends Parameter(name, t)
  //  case class InOutParameter(name: IdName, t: BasicType) extends Parameter(name, t)
  class Parameter(val idName: IdName)
  case class InParameter(name:IdName) extends Parameter(name)
  case class OutParameter(name:IdName) extends Parameter(name)



  lazy val parameters: PackratParser[List[Parameter]] = repsep(parameter, ",")
  //  lazy val inParameters: PackratReader[List[InParameter]] = repsep(parameter, ",")


  //  lazy val parameter: PackratParser[Parameter] =
  //    "in" ~> identifierName ~ (":" ~> declarationType) ^^ { case name~typ => InParameter(name, typ)} |
  //      "out" ~> identifierName ~ (":" ~> declarationType) ^^ { case name~typ => OutParameter(name, typ)} |
  //      "inout" ~> identifierName ~ (":" ~> declarationType) ^^ { case name~typ => InOutParameter(name, typ)} |
  //      identifierName ~ (":" ~> declarationType) ^^ { case name~typ => InOutParameter(name, typ)}
  lazy val parameter: PackratParser[Parameter] =
    identifierName  ^^ { case name => InParameter(name)} |
      identifierName ^^ { case name => OutParameter(name)}
  //    "inout" ~> identifierName ~ (":" ~> declarationType) ^^ { case name~typ => InOutParameter(name, typ)} |
  //    identifierName ^^ { case name => InOutParameter(name)}

  /******************************************************************************************/
  //  lazy val statement: PackratParser[Statement] =
  //    singleLineStmt |
  //      forStmt |
  //      ifStmt |
  //      whileStmt |
  //      doStmt |
  //      switchStmt |
  //      blockStmt
  lazy val statement: PackratParser[Statement] =
      statementCmd

//  lazy val statementExpr: PackratParser[Statement] = assignmentStmt
  //    simpleAssignment|
  //      multiAssignment


  lazy val statementCmd: PackratParser[Statement] =
   singleLineStmt|
    forStmt|
  whileStmt|
   switchStmt|
    ifStmt|
//    breakStmt|
//    continueStmt|
//    retStmt|
    function_def


  lazy val blockStmt : PackratParser[Statement] =
    (statement*) ^^ { case stmts => StatementBlock(stmts)}

  lazy val singleLineStmt: PackratParser[Statement] =
    declarationStmt <~ eol |
      assignmentStmt <~ eol |
      caseStmt <~ eol |
      retStmt <~ eol |
      defaultStmt <~ eol |
      breakStmt <~ eol |
      continueStmt <~ eol

  lazy val declarationStmt: PackratParser[Statement] =
//    "var" ~> repsep(declarator, ",") ^^ { case declarators => DeclarationStatement(declarators)}
  //"var" ~> declarator <~ eol ^^ { case declarator => DeclarationStatement(declarator)}
  assignmentStmt|
  "global"~> declarator ^^{case declarator=> DeclarationStatement(List(declarator))}|
  "local"~> declarator ^^{case declarator => DeclarationStatement(List(declarator)) }
  lazy val declarator: PackratParser[Declarator] = identifierName^^{case idname => Declarator(idname)}

  lazy val assignmentLHSexpr: PackratParser[Expr] =
    arrayRefExpr |
      identifier

  lazy val assignmentStmt: PackratParser[Statement] =
    assignmentLHSexpr ~ ("=" ~> expr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpAssign())} |
      assignmentLHSexpr ~ ("+=" ~> expr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpPlusAssign())} |
      assignmentLHSexpr ~ ("-=" ~> expr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpMinusAssign())} |
      assignmentLHSexpr ~ ("*=" ~> expr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpTimesAssign())} |
      assignmentLHSexpr ~ ("/=" ~> expr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpDivideAssign())}

  //  lazy val forInit: PackratParser[Statement] =
  //    "(" ~> assignmentStmt <~ ")" |
  //      assignmentStmt
  //
  //  lazy val forStmt: PackratParser[Statement] =
  //    ("for" ~> "(" ~> (forInit ~ (";" ~> simpleExpr) ~ (";" ~> simpleExprList) <~ ")")) ~ statement ^^ {
  //      case initStmt~condExpr~iterExpr~stmt => ForStatement(Some(initStmt), Some(condExpr), Some(iterExpr), stmt)
  //    }
  lazy val forStmt: PackratParser[Statement] =
    ("for"~> opt( multiAssignStmt)) ~blockStmt<~"end"^^{
      case range~body =>ForStatement(range,null,null, body)
    }

  lazy val ifStmt: PackratParser[Statement] =
    ("if" ~> expr ) ~ blockStmt ~ ("else" ~> blockStmt) <~"end"^^ { case cond~thebody~elsebody => IfStatement(cond, thebody, elsebody)} |
      //      ("if"~>expr) ~statement~repsep((("elseif" ~> expr) ~statement),whiteSpace)<~"end"^^ {case cond~elseifbody=>IfStatement(cond,)}                                |
      //Todo:elseif
      ("if" ~> expr) ~ blockStmt <~"end"^^ { case cond~stmt => IfStatement(cond, stmt)}
  //  lazy val elseifStmt: PackratParser[Statement] =
  //    ("elseif" ~> expr) ~statement

  lazy val whileStmt: PackratParser[Statement] =
    ("while"  ~> expr) ~ blockStmt <~"end"^^ { case cond~stmt => WhileStatement(cond, stmt)}

  //  lazy val doStmt: PackratParser[Statement] =
  //    ("do" ~> statement) ~  ("while" ~> "(" ~> expr <~ ")") ^^ { case stmt~cond => DoStatement(cond, stmt)}

  lazy val switchStmt: PackratParser[Statement] =
    ("switch" ~> expr ) ~ blockStmt<~"end" ^^ { case cond~stmt => SwitchStatement(cond, stmt)}

  lazy val breakStmt: PackratParser[Statement] =
    "break" ^^ {x => BreakStatement()}

  lazy val continueStmt: PackratParser[Statement] =
    "continue" ^^ {x => ContinueStatement()}

  lazy val defaultStmt: PackratParser[Statement] =
    "otherwise" ^^ {x => DefaultStatement()}

  lazy val caseStmt: PackratParser[Statement] =
    "case" ~> expr  ^^ {xpr => CaseStatement(xpr)}

  lazy val retStmt: PackratParser[Statement] =
    "return" ~> expr  ^^ {xpr => ReturnStatement(xpr)} |
      "return"  ^^ {x => ReturnStatement()}

  lazy val multiAssignStmt: PackratParser[Statement] =
    assignmentLHSexpr ~ ("=" ~> sliceExpr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpAssign())}
//      assignmentLHSexpr ~ ("+=" ~> sliceExpr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpPlusAssign())} |
//      assignmentLHSexpr ~ ("-=" ~> sliceExpr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpMinusAssign())} |
//      assignmentLHSexpr ~ ("*=" ~> sliceExpr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpTimesAssign())} |
//      assignmentLHSexpr ~ ("/=" ~> sliceExpr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpDivideAssign())}


  /******************************************************************************************/
  // Declaration
  //var T:Float[nsize][nsize]
  //var norm:Float

//  lazy val declarator: PackratParser[Declarator] =
//    identifierName ~ (":" ~> declarationType) ^^ { case name~typ => {
//      declMap+= name -> typ
//      Declarator(name)
//    }} |
//      identifierName ^^ { case name => Declarator(name)}




//  lazy val declarationType: PackratParser[BasicType] =
//    basicType ~ rep1("[" ~> simpleArithmeticExpr <~ "]") ^^ { case subtype~sizes => ArrayType(subtype, sizes.length, sizes) } |
//      basicType

//  lazy val basicType: PackratParser[BasicType] =
//    "Int" ^^ { case t => IntType()} |
//      "Float" ^^ { case t => FloatType()}

  /******************************************************************************************/
  lazy val expr: PackratParser[Expr] = arrayExpr


  //Array Expressions

  lazy val arrayExpr: PackratParser[Expr] =
    arrayConditionalExpr

  lazy val arrayConditionalExpr: PackratParser[Expr] =
    (arrayConditionalExpr <~ "&&") ~ arrayCompareExpr ^^ { case lhs~rhs => NAryExpr(OpLogicalAnd(), List(lhs, rhs)) } |
      (arrayConditionalExpr <~ "||") ~ arrayCompareExpr ^^ { case lhs~rhs => NAryExpr(OpLogicalOr(), List(lhs, rhs)) } |
      arrayCompareExpr

  lazy val arrayCompareExpr: PackratParser[Expr] =
    (arrayTerm <~ "==") ~ arrayTerm ^^ { case lhs~rhs => NAryExpr(OpEquals(), List(lhs, rhs)) } |
      (arrayTerm <~ "!=") ~ arrayTerm ^^ { case lhs~rhs => NAryExpr(OpNotEquals(), List(lhs, rhs)) } |
      (arrayTerm <~ "<") ~ arrayTerm ^^ { case lhs~rhs => NAryExpr(OpLessThan(), List(lhs, rhs)) } |
      (arrayTerm <~ "<=") ~ arrayTerm ^^ { case lhs~rhs => NAryExpr(OpLessEq(), List(lhs, rhs)) } |
      (arrayTerm <~ ">") ~ arrayTerm ^^ { case lhs~rhs => NAryExpr(OpGreaterThan(), List(lhs, rhs)) } |
      (arrayTerm <~ ">=") ~ arrayTerm ^^ { case lhs~rhs => NAryExpr(OpGreaterEq(), List(lhs, rhs)) } |
      arrayTerm

  lazy val arrayTerm: PackratParser[Expr] =
    (arrayTerm <~ "+") ~ arrayTerm2 ^^ { case lhs~rhs => NAryExpr(OpPlus(), List(lhs, rhs)) } |
      (arrayTerm <~ "-") ~ arrayTerm2 ^^ { case lhs~rhs => NAryExpr(OpMinus(), List(lhs, rhs)) } |
      arrayTerm2

  lazy val arrayTerm2: PackratParser[Expr] =
    (arrayTerm2 <~ "*") ~ arrayUnaryTerm ^^ { case lhs~rhs => NAryExpr(OpTimes(), List(lhs, rhs)) } |
      (arrayTerm2 <~ "/") ~ arrayUnaryTerm ^^ { case lhs~rhs => NAryExpr(OpDivide(), List(lhs, rhs)) } |
      (arrayTerm2 <~ "^") ~ arrayUnaryTerm ^^ { case lhs~rhs => NAryExpr(OpPow(), List(lhs, rhs)) } |
      (arrayTerm2 <~ ".*") ~ arrayUnaryTerm ^^ { case lhs~rhs => NAryExpr(OpDotProd(), List(lhs, rhs)) } |
      (arrayTerm2 <~ "<o>") ~ arrayUnaryTerm ^^ { case lhs~rhs => NAryExpr(OpOuterProd(), List(lhs, rhs)) } |
      (arrayTerm2 <~ "**") ~ arrayUnaryTerm ^^ { case lhs~rhs => NAryExpr(OpMatProd(), List(lhs, rhs)) } |
      (arrayTerm2 <~ "*^") ~ arrayUnaryTerm ^^ { case lhs~rhs => NAryExpr(OpMatPow(), List(lhs, rhs)) } |
      arrayUnaryTerm

  lazy val arrayUnaryTerm: PackratParser[Expr] =
    sumTerm | prodTerm | permuteTerm |
      identifier ~ ("(" ~> repsep(arrayTerm, ",") <~ ")") ^^ { case name~params => FunctionCallExpr(name,params)} |
      "(" ~> arrayExpr <~ ")" |
      arrayFactor
//      "-" ~> arrayTerm ^^ { case x => UnaryExpr(OpNegate(), x) }

  lazy val sumTerm: PackratParser[Expr] =
    ("Sum(" ~> arrayTerm <~ ",") ~ integerList <~ ")" ^^ { case xpr~list => UnaryExpr(OpSum(list), xpr) } |
      "Sum(" ~> arrayTerm <~ ")" ^^ { case xpr => UnaryExpr(OpSum(List.empty[Int]), xpr) }

  lazy val prodTerm: PackratParser[Expr] =
    ("Prod(" ~> arrayTerm <~ ",") ~ integerList <~ ")" ^^ { case xpr~list => UnaryExpr(OpProd(list), xpr) } |
      "Prod(" ~> arrayTerm <~ ")" ^^ { case xpr => UnaryExpr(OpProd(List.empty[Int]), xpr) }

  lazy val permuteTerm: PackratParser[Expr] =
    ("Permute(" ~> arrayTerm <~ ",") ~ integerList <~ ")" ^^ { case xpr~list => UnaryExpr(OpPermute(list), xpr) }

  lazy val arrayFactor : PackratParser[Expr] =
    arrayRefExpr |
      simpleFactor

  /******************************************************************************************/
  // Array Reference

  lazy val arrayRefExpr: PackratParser[ArrayRefExpr] =
    identifier ~ rep1("(" ~> sliceExpr <~ ")") ^^ { case id~slices => ArrayRefExpr(id, slices)}

  lazy val sliceExpr: PackratParser[Expr] =
    simpleExpr ~ (":" ~> simpleExpr) ~ (":" ~> simpleExpr) ^^ { case lower~upper~stride => SliceExpr(lower, upper, stride)} |
      simpleExpr ~ (":" ~> simpleExpr) ^^ { case lower~upper => SliceExpr(lower, upper)} |
      simpleExpr

  /******************************************************************************************/
  // Simple Expressions
  lazy val simpleExprList: PackratParser[Expr] =
    repsep(simpleExpr, ",") ^^ { case exprs => exprs.size match {
      case 0 => throw new UnsupportedOperationException("ExpressionList of size 0")
      case 1 => exprs(0)
      case _ => ExpressionListExpr(exprs)
    } } |
      simpleExpr

  lazy val simpleExpr: PackratParser[Expr] =
    simpleConditionalExpr

  lazy val simpleConditionalExpr: PackratParser[Expr] =
    (simpleConditionalExpr <~ "&&") ~ simpleCompareExpr ^^ { case lhs~rhs => NAryExpr(OpLogicalAnd(), List(lhs, rhs)) } |
      (simpleConditionalExpr <~ "||") ~ simpleCompareExpr ^^ { case lhs~rhs => NAryExpr(OpLogicalOr(), List(lhs, rhs)) } |
      simpleCompareExpr

  lazy val simpleCompareExpr: PackratParser[Expr] =
    (simpleArithmeticExpr <~ "==") ~ simpleArithmeticExpr ^^ { case lhs~rhs => NAryExpr(OpEquals(), List(lhs, rhs)) } |
      (simpleArithmeticExpr <~ "!=") ~ simpleArithmeticExpr ^^ { case lhs~rhs => NAryExpr(OpNotEquals(), List(lhs, rhs)) } |
      (simpleArithmeticExpr <~ "<") ~ simpleArithmeticExpr ^^ { case lhs~rhs => NAryExpr(OpLessThan(), List(lhs, rhs)) } |
      (simpleArithmeticExpr <~ "<=") ~ simpleArithmeticExpr ^^ { case lhs~rhs => NAryExpr(OpLessEq(), List(lhs, rhs)) } |
      (simpleArithmeticExpr <~ ">") ~ simpleArithmeticExpr ^^ { case lhs~rhs => NAryExpr(OpGreaterThan(), List(lhs, rhs)) } |
      (simpleArithmeticExpr <~ ">=") ~ simpleArithmeticExpr ^^ { case lhs~rhs => NAryExpr(OpGreaterEq(), List(lhs, rhs)) } |
      simpleArithmeticExpr

  lazy val simpleArithmeticExpr: PackratParser[Expr] =
    (simpleArithmeticExpr <~ "+") ~ simpleTerm ^^ { case lhs~rhs => NAryExpr(OpPlus(), List(lhs, rhs)) } |
      (simpleArithmeticExpr <~ "-") ~ simpleTerm ^^ { case lhs~rhs => NAryExpr(OpMinus(), List(lhs, rhs)) } |
      simpleTerm

  lazy val simpleTerm: PackratParser[Expr] =
    (simpleTerm <~ "*") ~ simpleUnaryTerm ^^ { case lhs~rhs => NAryExpr(OpTimes(), List(lhs, rhs)) } |
      (simpleTerm <~ "/") ~ simpleUnaryTerm ^^ { case lhs~rhs => NAryExpr(OpDivide(), List(lhs, rhs)) } |
      simpleUnaryTerm

  lazy val simpleUnaryTerm: PackratParser[Expr] =
    "++" ~> simpleFactor ^^ { case x => UnaryExpr(OpPrefixInc(), x)} |
      "--" ~> simpleFactor ^^ { case x => UnaryExpr(OpPrefixDec(), x)} |
      simpleFactor <~ "++" ^^ { case x => UnaryExpr(OpPostfixInc(), x)} |
      simpleFactor <~ "--" ^^ { case x => UnaryExpr(OpPostfixDec(), x)} |
      "(" ~> simpleExpr <~ ")" |
      simpleFactor

  lazy val simpleFactor : PackratParser[Expr] =
    const_literal |
      identifier

  /******************************************************************************************/
  lazy val const_literal: PackratParser[ConstLiteralExpr] =
    stringLiteral ^^ {x => ConstLiteralExpr(x)} |
      floatingPointNumber ^^ {x => toFloat(x)} |
      decimalNumber ^^ {x => toFloat(x)} |
      wholeNumber ^^ {x => ConstLiteralExpr(x.toInt)} |
      booleanLiteral

  def toFloat(x: String) = {
    try {
      ConstLiteralExpr(x.toInt)
    } catch {
      case e: NumberFormatException => ConstLiteralExpr(x.toFloat)
    }
  }

  //[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?

  lazy val booleanLiteral: PackratParser[ConstLiteralExpr] =
    ("true" | "false") ^^ {x => ConstLiteralExpr(x.toBoolean)}

  lazy val integerList: PackratParser[List[Int]] =  "{" ~> repsep(integer, ",") <~ "}"
  lazy val integer: PackratParser[Int] = wholeNumber ^^ {x => x.toInt}

  lazy val identifier: PackratParser[IdExpr] = identifierName ^^ {x => IdExpr(x)}

  lazy val identifierName: PackratParser[IdName] = "[a-zA-Z][a-zA-Z0-9_]*".r ^^ {x => IdName(x)}
  //lazy val identifierName: PackratParser[IdName] = not(keywords)~ident ^^ { case x~name => IdName(name)}

  //override val whiteSpace = """(\s|(//.*\n))+""".r

  //Ignores C-style comments
  //override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
  override val whiteSpace = """(\s|%.*|%\{(\n|\r)+\}%)+""".r


  /******************************************************************************************/
  // matrix	: LSBRACE vector? ( nlos vector )* RSBRACE -> ^(MATRIX vector*);
  //vector	:	expression ( COMMA expression )* -> ^(VECTOR expression+);
  //lazy val vector : PackratParser[VectorExpr] =
  //  lazy val matrix : PackratParser[MatrixExpr] =
//  lazy val cell: PackratParser[]

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
      case Success (statement, _) => Left(declMap.toMap, statement)
      //      case Success(function_def,_)=>Left( parseFunctions(text),function_def)
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
}
