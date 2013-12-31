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

object MatlabParser extends JavaTokenParsers with PackratParsers {
  val declMap = HashMap[IdName, BasicType]()

  //lazy val keywords: PackratParser[Any] = "var" | "for"
  lazy val eol: Parser[Any] = rep(""";(\r?\n)?+""".r)

  lazy val source: PackratParser[StatementBlock] =
    (statement*) ^^ { case stmts => StatementBlock(stmts)}

  lazy val functions: PackratParser[List[FunctionDef]] = rep(function)

  def newFunction(name: IdName, paramsList: List[Parameter], body: Statement, typ: Option[BasicType]): FunctionDef = {
    val inParams = ListBuffer[IdName]()
    val outParams = ListBuffer[IdName]()
    val params = ListBuffer[IdName]()
    paramsList.foreach(param => {
      declMap += param.idName -> param.typ
      params += param.idName
      param match {
        case p: InParameter => inParams += p.idName
        case p: OutParameter => outParams += p.idName
        case p: InOutParameter => {
          inParams += p.idName
          outParams += p.idName
        }
      }
    })

    FunctionDef(name, params.toList, inParams.toList, outParams.toList, body, typ)
  }

  lazy val function: PackratParser[FunctionDef] =
    "def" ~> identifierName ~ ("(" ~> parameters <~ ")") ~ (":" ~> declarationType) ~ blockStmt ^^ {
      case name~params~typ~body => newFunction(name, params, body, Some(typ))
    } |
      "def" ~> identifierName ~ ("(" ~> parameters <~ ")") ~ blockStmt ^^ {
        case name~params~body => newFunction(name, params, body, None)
      }

  class Parameter(val idName: IdName, val typ: BasicType)
  case class InParameter(name: IdName, t: BasicType) extends Parameter(name, t)
  case class OutParameter(name: IdName, t: BasicType) extends Parameter(name, t)
  case class InOutParameter(name: IdName, t: BasicType) extends Parameter(name, t)

  lazy val parameters: PackratParser[List[Parameter]] = repsep(parameter, ",")

  lazy val parameter: PackratParser[Parameter] =
    "in" ~> identifierName ~ (":" ~> declarationType) ^^ { case name~typ => InParameter(name, typ)} |
      "out" ~> identifierName ~ (":" ~> declarationType) ^^ { case name~typ => OutParameter(name, typ)} |
      "inout" ~> identifierName ~ (":" ~> declarationType) ^^ { case name~typ => InOutParameter(name, typ)} |
      identifierName ~ (":" ~> declarationType) ^^ { case name~typ => InOutParameter(name, typ)}

  /******************************************************************************************/
  lazy val statement: PackratParser[Statement] =
    singleLineStmt |
      forStmt |
      ifStmt |
      whileStmt |
      doStmt |
      switchStmt |
      blockStmt

  lazy val blockStmt : PackratParser[Statement] =
    "{" ~> (statement*) <~ "}" ^^ { case stmts => StatementBlock(stmts)}

  lazy val singleLineStmt: PackratParser[Statement] =
    declarationStmt <~ eol |
      assignmentStmt <~ eol |
      caseStmt <~ eol |
      retStmt <~ eol |
      defaultStmt <~ eol |
      breakStmt <~ eol |
      continueStmt <~ eol

  lazy val declarationStmt: PackratParser[Statement] =
    "var" ~> repsep(declarator, ",") ^^ { case declarators => DeclarationStatement(declarators)}
  //"var" ~> declarator <~ eol ^^ { case declarator => DeclarationStatement(declarator)}

  lazy val assignmentLHSexpr: PackratParser[Expr] =
    arrayRefExpr |
      identifier

  lazy val assignmentStmt: PackratParser[Statement] =
    assignmentLHSexpr ~ ("=" ~> expr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpAssign())} |
      assignmentLHSexpr ~ ("+=" ~> expr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpPlusAssign())} |
      assignmentLHSexpr ~ ("-=" ~> expr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpMinusAssign())} |
      assignmentLHSexpr ~ ("*=" ~> expr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpTimesAssign())} |
      assignmentLHSexpr ~ ("/=" ~> expr) ^^ { case lhs~rhs => AssignmentStatement(lhs, rhs, OpDivideAssign())}

  lazy val forInit: PackratParser[Statement] =
    "(" ~> assignmentStmt <~ ")" |
      assignmentStmt

  lazy val forStmt: PackratParser[Statement] =
    ("for" ~> "(" ~> (forInit ~ (";" ~> simpleExpr) ~ (";" ~> simpleExprList) <~ ")")) ~ statement ^^ {
      case initStmt~condExpr~iterExpr~stmt => ForStatement(Some(initStmt), Some(condExpr), Some(iterExpr), stmt)
    }

  lazy val ifStmt: PackratParser[Statement] =
    ("if" ~> "(" ~> expr <~ ")") ~ statement ~ ("else" ~> statement) ^^ { case cond~thebody~elsebody => IfStatement(cond, thebody, elsebody)} |
      ("if" ~> "(" ~> expr <~ ")") ~ statement ^^ { case cond~stmt => IfStatement(cond, stmt)}

  lazy val whileStmt: PackratParser[Statement] =
    ("while" ~> "(" ~> expr <~ ")") ~ statement ^^ { case cond~stmt => WhileStatement(cond, stmt)}

  lazy val doStmt: PackratParser[Statement] =
    ("do" ~> statement) ~  ("while" ~> "(" ~> expr <~ ")") ^^ { case stmt~cond => DoStatement(cond, stmt)}

  lazy val switchStmt: PackratParser[Statement] =
    ("switch" ~> "(" ~> expr <~ ")") ~ statement ^^ { case cond~stmt => SwitchStatement(cond, stmt)}

  lazy val breakStmt: PackratParser[Statement] =
    "break" ^^ {x => BreakStatement()}

  lazy val continueStmt: PackratParser[Statement] =
    "continue" ^^ {x => ContinueStatement()}

  lazy val defaultStmt: PackratParser[Statement] =
    "default:" ^^ {x => DefaultStatement()}

  lazy val caseStmt: PackratParser[Statement] =
    "case" ~> simpleExpr <~ ":" ^^ {xpr => CaseStatement(xpr)}

  lazy val retStmt: PackratParser[Statement] =
    "return" ~> simpleExpr  ^^ {xpr => ReturnStatement(xpr)} |
      "return"  ^^ {x => ReturnStatement()}

  /******************************************************************************************/
  // Declaration
  //var T:Float[nsize][nsize]
  //var norm:Float

  lazy val declarator: PackratParser[Declarator] =
    identifierName ~ (":" ~> declarationType) ^^ { case name~typ => {
      declMap+= name -> typ
      Declarator(name)
    }} |
      identifierName ^^ { case name => Declarator(name)}


  lazy val declarationType: PackratParser[BasicType] =
    basicType ~ rep1("[" ~> simpleArithmeticExpr <~ "]") ^^ { case subtype~sizes => ArrayType(subtype, sizes.length, sizes) } |
      basicType

  lazy val basicType: PackratParser[BasicType] =
    "Int" ^^ { case t => IntType()} |
      "Float" ^^ { case t => FloatType()}

  /******************************************************************************************/
  lazy val expr: PackratParser[Expr] = arrayExpr

  //Array Expressions

  lazy val arrayExpr: PackratParser[Expr] =
    arrayCondtionalExpr

  lazy val arrayCondtionalExpr: PackratParser[Expr] =
    (arrayCondtionalExpr <~ "&&") ~ arrayCompareExpr ^^ { case lhs~rhs => NAryExpr(OpLogicalAnd(), List(lhs, rhs)) } |
      (arrayCondtionalExpr <~ "||") ~ arrayCompareExpr ^^ { case lhs~rhs => NAryExpr(OpLogicalOr(), List(lhs, rhs)) } |
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
      arrayFactor |
      "-" ~> arrayTerm ^^ { case x => UnaryExpr(OpNegate(), x) }

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
    identifier ~ rep1("[" ~> sliceExpr <~ "]") ^^ { case id~slices => ArrayRefExpr(id, slices)}

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
  lazy val vector : PackratParser[]
  lazy val matrix : PackratParser[]

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
