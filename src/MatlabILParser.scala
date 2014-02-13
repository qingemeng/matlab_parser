/**
* Created with IntelliJ IDEA.
* User: gemengqin
* Date: 12/26/13
* Time: 4:52 PM
* To change this template use File | Settings | File Templates.
*/



import scala.xml._
import model._
import model.expression._
import model.statement._
import refactoring.matlab.model._

object MatlabILParser {
  def parse(script: Elem) = {
    if(script.label == "matlab_script"){
      val scriptBody = script \ "matlab_script__body"

      if(!scriptBody.isEmpty){
        parseStatementList(getFirstChild(scriptBody(0)))
      } else {
        throw new RuntimeException("Empty Matlab Script: " + script)
      }
    } else {
      throw new RuntimeException("Not Matlab Script: " + script)
    }
  }

  def parseStatementList(node: Node):StatementBlock = {
    if(node.label == "tree_statement_list"){
      val dslStmtBlk = StatementBlock()

      val liststatements = node \ "tree_statement_list__statement"
      liststatements.foreach(liststmt => {
        val stmt = liststmt \ "tree_statement"
        val dslStmt = parseStatement(stmt(0))
        if(!dslStmt.isInstanceOf[NullStatement])
          dslStmtBlk.appendStatement(dslStmt)
      })
      dslStmtBlk
    } else {
      throw new RuntimeException("Not Statement List: " + node.label)
    }
  }

  def parseStatement(node: Node):Statement = {
    if(node.label == "tree_statement"){
      val firstChild = getFirstChild(node)
      firstChild.label match {
        case "tree_statement__expression"	=> {
          val stmtExpr = getFirstChild(firstChild)

          stmtExpr.label match {
            case "tree_simple_assignment" => parseSimpleAssignment(stmtExpr)
            case "tree_multi_assignment"  => parseMultiAssignment(stmtExpr)
            case _ => ExpressionStatement(parseExpression(stmtExpr))
          }
        }
        case "tree_statement__command" => {
          val stmtCmd = getFirstChild(firstChild)

          stmtCmd.label match {
            case "tree_simple_for_command"	=> parseForCommand(stmtCmd)
            case "tree_while_command"		=> parseWhileCommand(stmtCmd)
            case "tree_switch_command"		=> parseSwitchCommand(stmtCmd)
            case "tree_if_command"			=> parseIfCommand(stmtCmd)
            case "tree_function_def"		=> parseFunction(getFirstChild(stmtCmd))
            case "tree_static_command"		=> parseStaticCommand(stmtCmd)
            case "tree_global_command"		=> parseGlobalCommand(stmtCmd)
            case "tree_break_command"		=> BreakStatement()
            case "tree_continue_command"	=> ContinueStatement()
            case "tree_return_command"		=> ReturnStatement()
            case "tree_no_op_command"		=> NullStatement()

            case "tree_unwind_protect_command" => throw new UnsupportedOperationException("unwind_protect")
            case "tree_try_catch_command"	=>  throw new UnsupportedOperationException("try_catch")

            case _ => throw new RuntimeException("Unknown model.statement command: " + node)
          }
        }
      }
    } else {
      throw new RuntimeException("Not model.statement: " + node.label)
    }
  }

  def parseFunction(node: Node): FunctionDefStatement = {
    if(node.label == "octave_user_function"){
      val funcHeaders = node \ "octave_user_function_header"

      val returnLists = funcHeaders(0) \ "octave_user_function_header__return_list"
      val parameterLists = funcHeaders(0) \ "octave_user_function_header__param_list"
      val headerNames = funcHeaders(0) \ "octave_user_function_header__name"

      def parseParameterList(node: Node) =  parseListElements(node \ "tree_parameter_list__element")

      val dslOutput = {
        if(returnLists.isEmpty){
          List()
        } else {
          val declarators = parseParameterList(getFirstChild(returnLists(0)))
          declarators.map(_.idName)
        }
      }

      val dslInput = {
        if(parameterLists.isEmpty){
          List()
        } else {
          val declarators = parseParameterList(getFirstChild(parameterLists(0)))
          declarators.map(_.idName)
        }
      }

      val dslParameters = dslInput
      val dslName = IdName(headerNames(0).text)

      val funcBody = node \ "octave_user_function__body" \ "tree_statement_list"
      val dslStmts = parseStatementList(funcBody(0))

      val funcDef = FunctionDef(dslName, dslParameters, dslInput, dslOutput, dslStmts)
      FunctionDefStatement(funcDef)
    } else {
      throw new RuntimeException("Not User Function: " + node.label)
    }
  }

  def parseListElements(listElems: NodeSeq): List[Declarator] = {
    listElems.map(listElem => {
      val declElem = listElem \ "tree_decl_elt"
      val ident = declElem \ "tree_decl_elt__ident"
      val expr = declElem \ "tree_decl_elt__expression"

      val dslId = parseId(getFirstChild(ident(0)))
      if(!expr.isEmpty){
        val dslExpr = parseExpression(getFirstChild(expr(0)))
        Declarator(dslId, dslExpr)
      } else {
        Declarator(dslId)
      }
    }).toList
  }

  //DeclType: static, global
  def parseDeclarationCommand(declType:String, node: Node) = {
    if(node.label == "tree_" + declType +"_command"){
      val initList = node \ ("tree_"+declType+"_command__initializer_list") \ "tree_decl_init_list"
      def parseInitList(node: Node) =  parseListElements(node \ "tree_decl_init_list__element")

      val dslList = parseInitList(getFirstChild(initList(0)))
      DeclarationStatement(dslList)
    } else {
      throw new RuntimeException("Not static command: " + node.label)
    }
  }
  def parseStaticCommand(node: Node) = parseDeclarationCommand("static", node)
  def parseGlobalCommand(node: Node) = parseDeclarationCommand("global", node)

  def parseSimpleAssignment(node: Node) = {
    if(node.label == "tree_simple_assignment"){
      val lhs = node \ "tree_simple_assignment__left_hand_side"
      val ops = node \ "tree_simple_assignment__operator"
      val rhs = node \ "tree_simple_assignment__right_hand_side"

      val dslLhs = parseExpression(getFirstChild(lhs(0)))
      val dslOp = assignOp(ops(0).text)
      val dslRhs = parseExpression(getFirstChild(rhs(0)))

      AssignmentStatement(dslLhs, dslRhs, dslOp)
    } else {
      throw new RuntimeException("Not simple assignment: " + node.label)
    }
  }

  def parseMultiAssignment(node: Node) = {
    if(node.label == "tree_multi_assignment"){
      val lhs = node \ "tree_multi_assignment__left_hand_side"
      val ops = node \ "tree_multi_assignment__operator"
      val rhs = node \ "tree_multi_assignment__right_hand_side"

      val dslLhs = parseArgumentList(getFirstChild(lhs(0)))
      val dslOp = assignOp(ops(0).text)
      val dslRhs = parseExpression(getFirstChild(rhs(0)))

      AssignmentStatement(TupleExpr(dslLhs), dslRhs, dslOp)
    } else {
      throw new RuntimeException("Not multi assignment: " + node.label)
    }
  }

  def parseForCommand(node: Node):ForStatement = {
    if(node.label == "tree_simple_for_command"){
      // check if there is stencil tag
      val stencil = node \ "octave_comment_list" \ "octave_comment_elt"
      val isStencil = (stencil.size != 0) && (stencil.text.contains("!$stencil"))

      val lhs = node \ "tree_simple_for_command__left_hand_side"
      val cond = node \ "tree_simple_for_command__control_expr"
      val body = node \ "tree_simple_for_command__body"

      val dslLhs = parseExpression(getFirstChild(lhs(0)))
      val dslCond = parseExpression(getFirstChild(cond(0)))
      val dslBody = parseStatementList(getFirstChild(body(0)))

      if(dslCond.isInstanceOf[SliceExpr]){
        val slice = dslCond.asInstanceOf[SliceExpr]

        val initExpr = NAryExpr(OpTempAssign(), List(dslLhs.cloneExpr(), slice.lowerBound))
        val condExpr = NAryExpr(OpLessEq(), List(dslLhs.cloneExpr(), slice.upperBound))
        val iterExpr = NAryExpr(OpTempPlusAssign(), List(dslLhs.cloneExpr(), slice.stride))

        val forStmt = ForStatement(ExpressionStatement(initExpr), Some(condExpr), Some(iterExpr), dslBody)
        forStmt.isStencil = isStencil
        forStmt
      } else {
        val forStmt = ForStatement(ExpressionStatement(dslCond), None, None, dslBody)
        forStmt.isStencil = isStencil
        forStmt
      }
    } else {
      throw new RuntimeException("Not For Command: " + node.label)
    }
  }

  def parseWhileCommand(node:Node):WhileStatement = {
    if(node.label == "tree_while_command"){
      val cond = node \ "tree_while_command__condition"
      val body = node \ "tree_while_command__body"

      val dslCond = parseExpression(getFirstChild(cond(0)))
      val dslBody = parseStatementList(getFirstChild(body(0)))

      WhileStatement(dslCond, dslBody)
    } else {
      throw new RuntimeException("Not While Command: " + node.label)
    }
  }

  def parseIfCommand(node: Node):IfStatement = {
    if(node.label == "tree_if_command"){
      val ifs = node \ "tree_if_command__cmd_list" \ "tree_if_command_list" \ "tree_if_command_list__if_clause"

      var parentIf:IfStatement = null
      ifs.foreach(ifStmt => {
        val ifType = (ifStmt \ "@type").toString()

        val clause = ifStmt \ "tree_if_clause"
        val body = clause \ "tree_if_clause__commands"
        val dslBody = parseStatementList(getFirstChild(body(0)))

        if(ifType == "if"){
          val condition = clause \ "tree_if_clause__condition"
          val dslCond = parseExpression(getFirstChild(condition(0)))

          if(parentIf == null){
            parentIf = IfStatement(dslCond, dslBody)
          } else {
            //val currentIf = IfStatement(dslCond, dslBody, parentIf)
            //parentIf.update(elseBody = Some(currentIf))
            //parentIf = currentIf
          }
        } else if(ifType == "else"){
          parentIf.update(elseBody = Some(dslBody))
        }
      })

      parentIf
    } else {
      throw new RuntimeException("Not If Command: " + node.label)
    }
  }

  def parseSwitchCommand(node: Node):SwitchStatement = {
    if(node.label == "tree_switch_command"){
      val switchValue = node \ "tree_switch_command__switch_value"
      val cases = node \ "tree_switch_command__case_list" \ "tree_switch_case_list" \ "tree_switch_case_list__case" \ "tree_switch_case"

      val cond = parseExpression(getFirstChild(switchValue(0)))
      val body = StatementBlock()
      cases.foreach(c => {
        val casecase = c \ "tree_switch_case__case"
        val caseType = casecase \ "@type"
        val cmd = casecase \ "tree_switch_case__commands"

        val dslBody = parseStatementList(getFirstChild(cmd(0)))

        if(caseType == "case"){
          val label = casecase \ "tree_switch_case__case_label"
          val caseStmt = CaseStatement(parseExpression(getFirstChild(label(0))))
          body.appendStatement(caseStmt)
        } else {
          val defaultStmt = DefaultStatement()
          body.appendStatement(defaultStmt)
        }
        body.appendStatement(dslBody)
        body.appendStatement(BreakStatement())
      })
      SwitchStatement(cond, body)
    } else {
      throw new RuntimeException("Not Switch Command: " + node.label)
    }
  }

  val functions: Set[String] = Set("exp", "cos", "sin", "tan", "log", "log10", "zeros", "ones", "floor", "ceil")
  def parseExpression(node: Node):Expr = node.label match {
    case "tree_index_expression" => {
      val subexprs = node \ "tree_index_expression__expression"
      val argumentList = node \ "tree_index_expression__paren_arguments" \ "tree_argument_list"
      if(!subexprs.isEmpty){
        val nameExpr = parseExpression(getFirstChild(subexprs(0)))

        //TODO: Determine whether ArrayRef or FunctionCall
        val parameters = parseArgumentList(argumentList(0))
        if (functions contains nameExpr.pretty()) {
          FunctionCallExpr(nameExpr, parameters)
        } else {
          ArrayRefExpr(nameExpr, parameters)
        }
        //        if(!argumentList.isEmpty){
        //          val parameters = parseArgumentList(argumentList(0))
        //          ArrayRefExpr(nameExpr, parameters)
        //        } else {
        //          ArrayRefExpr(nameExpr, List())
        //        }
      } else {
        throw new RuntimeException("Empty tree_index_expression__expression: " + node)
      }
    }

    case "tree_binary_expression" => {
      val lhs = node \ "tree_binary_expression__lhs"
      val ops = node \ "tree_binary_expression__operator"
      val rhs = node \ "tree_binary_expression__rhs"

      val dslLhs = parseExpression(getFirstChild(lhs(0)))
      val dslRhs = parseExpression(getFirstChild(rhs(0)))

      def inv(expr: Expr) = FunctionCallExpr(IdExpr(IdName("inv")), List(expr))
      def pinv(expr: Expr) = FunctionCallExpr(IdExpr(IdName("pinv")), List(expr))

      ops(0).text match {
        //case "/"	=> NAryExpr(OpTimes(), List(dslRhs, inv(dslLhs)))
        case "/"	=> NAryExpr(OpDivide(), List(dslLhs, dslRhs))
        case "./"	=> NAryExpr(OpDivide(), List(dslLhs, dslRhs))
        case "\\"	=> NAryExpr(OpTimes(), List(pinv(dslLhs), dslRhs))
        case ".\\"	=> NAryExpr(OpDivide(), List(dslRhs, dslLhs))
        case op		=> NAryExpr(binaryOp(op), List(dslLhs, dslRhs))
      }
    }

    case "tree_colon_expression" => {
      val base = node \ "tree_colon_expression__base"
      val inc = node \ "tree_colon_expression__increment"
      val limit = node \ "tree_colon_expression__limit"

      val dslLb = parseExpression(getFirstChild(base(0)))
      val dslUb = parseExpression(getFirstChild(limit(0)))

      if(!inc.isEmpty){
        val dslInc = parseExpression(getFirstChild(inc(0)))
        SliceExpr(dslLb, dslUb, dslInc)
      } else {
        SliceExpr(dslLb, dslUb)
      }
    }

    case "tree_constant" => {
      def parseConstant(text: String) = {
        //String Constant
        if(text.startsWith("\"") || text.startsWith("'")){
          ConstLiteralExpr(text.substring(1, text.length - 1))
        } else if(text.contains(":")){
          val subexprs = text.split(":")
          val indices = subexprs.map(e => ConstLiteralExpr(e.toInt))
          indices.size match {
            case 0 => SliceExpr(ConstLiteralExpr(1), ArrayEndExpr())

            case 2 => SliceExpr(indices(0), indices(1))
            case 3 => SliceExpr(indices(0), indices(1), indices(2))
          }

        } else {
          try {
            if(text.contains(".") || text.contains("e")){
              //Float Expression
              ConstLiteralExpr(text.toFloat)
            } else if(text.contains("i") || text.contains("j")){
              //Complex Expression
              ConstLiteralExpr(text)
            } else {
              //Integer Expression
              ConstLiteralExpr(text.toInt)
            }
          } catch {
            case e: NumberFormatException => ConstLiteralExpr(text)
          }
        }
      }

      if( (node.text.startsWith("[") && node.text.endsWith("]")) || node.text.startsWith("{") && node.text.endsWith("}")){
        val trimmedText = node.text.substring(1, node.text.length - 1)
        val subexprs = trimmedText.split(",").filter(_.length > 0)
        val dslExprs = subexprs.map(expr => parseConstant(expr)).toList
        ArrayCompositionExpr(dslExprs)
      } else {
        parseConstant(node.text)
      }
    }

    case "tree_postfix_expression" => {
      val operand = node \ "tree_postfix_expression__operand"
      val operator = node \ "tree_postfix_expression__operator"

      val dslOperand = parseExpression(getFirstChild(operand(0)))

      operator(0).text match {
        case "++" => UnaryExpr(OpPostfixInc(), dslOperand)
        case "--" => UnaryExpr(OpPostfixDec(), dslOperand)
        case "'"  => UnaryExpr(OpTranspose(), UnaryExpr(OpConj(), dslOperand))
        case ".'"  => UnaryExpr(OpTranspose(), dslOperand)

        case o => throw new RuntimeException("Unknown postfix operator: " + o)
      }
    }

    case "tree_prefix_expression" => {
      val operand = node \ "tree_prefix_expression__operand"
      val operator = node \ "tree_prefix_expression__operator"

      val dslOperand = parseExpression(getFirstChild(operand(0)))

      operator(0).text match {
        case "++" => UnaryExpr(OpPrefixInc(), dslOperand)
        case "--" => UnaryExpr(OpPrefixDec(), dslOperand)
        case "-"  => UnaryExpr(OpNegate(), dslOperand)
        case "!"  => UnaryExpr(OpNot(), dslOperand)
        case "+"  => dslOperand
        case o => throw new RuntimeException("Unknown prefix operator: " + o)
      }
    }

    case "tree_matrix" => {
      val argumentList = node \ "tree_matrix__arguments" \ "tree_argument_list"

      if(!argumentList.isEmpty){
        val parameters = parseArgumentList(argumentList(0))
        ArrayCompositionExpr(parameters)
      } else {
        ArrayCompositionExpr()
      }
    }

    case "tree_cell" => {
      val argumentList = node \ "tree_cell__arguments" \ "tree_argument_list"

      if(!argumentList.isEmpty){
        val parameters = parseArgumentList(argumentList(0))
        ArrayCompositionExpr(parameters)
      } else {
        ArrayCompositionExpr()
      }
    }

    case "tree_simple_assignment" => {
      val lhs = node \ "tree_simple_assignment__left_hand_side"
      val ops = node \ "tree_simple_assignment__operator"
      val rhs = node \ "tree_simple_assignment__right_hand_side"

      val dslLhs = parseExpression(getFirstChild(lhs(0)))
      val dslOp = ops(0).text match {
        case "=" => OpTempAssign()
        case "+=" => OpTempPlusAssign()
        case "-=" => OpTempMinusAssign()
        case "*=" => OpTempTimesAssign()
        case "/=" => OpTempDivideAssign()

        case op => throw new RuntimeException("Unknown assignment operator: " + op)
      }
      val dslRhs = parseExpression(getFirstChild(rhs(0)))

      NAryExpr(dslOp, List(dslLhs, dslRhs))
    }

    case "tree_identifier" => IdExpr(parseId(node))

    case "tree_fcn_handle" => throw new UnsupportedOperationException("Function Handles")
    case "tree_anon_fcn_handle" => throw new UnsupportedOperationException("Anonymous Function")

    case e => throw new RuntimeException("Unknown model.expression: " + node)
  }

  def parseArgumentList(node: Node): List[Expr] = {
    if(node.label == "tree_argument_list"){
      val arguments = node \ "tree_argument_list__argument"
      arguments.map(argument => parseExpression(getFirstChild(argument))).toList
    } else {
      throw new RuntimeException("Not Argument List: " + node)
    }
  }

  def parseId(node: Node):IdName = {
    if(node.label == "tree_identifier"){
      IdName(node.text)
    } else {
      throw new RuntimeException("Unknown Id: " + node.label)
    }
  }

  def getFirstChild(node: Node) = {
    var filt = node.child.filter(n => n.label != "#PCDATA" && n.label != "octave_comment_list")
    filt(0)
  }

  def binaryOp(op: String): NAryOp = op match {
    //Element Wise Op
    case ".+" | "+" => OpPlus()
    case ".-" | "-" => OpMinus()
    case ".*" => OpTimes()
    case "./" | "/" => OpDivide()
    case ".\\" | "\\" => OpLeftDivide()
    case ".^" => OpPow()
    case ".**" => OpPow()

//    case "*" => OpMatProd()
    case "**" => OpMatPow()
    case "^" => OpMatPow()

    case "<" => OpLessThan()
    case "<=" => OpLessEq()
    case ">" => OpGreaterThan()
    case ">=" => OpGreaterEq()
    case "==" => OpEquals()
    case "~=" => OpNotEquals()
    case "!=" => OpNotEquals()

    case "|" | "||" => OpLogicalOr()
    case "&" | "&&" => OpLogicalAnd()

//    case "<<" => OpShiftLeft()
//    case ">>" => OpShiftRight()

    case _ => throw new RuntimeException("Unknown binary operator: " + op)
  }

  def assignOp(op: String): AssignOp = op match {
    case "=" => OpAssign()
    case "+=" | ".+=" => OpPlusAssign()
    case "-=" | ".-=" => OpMinusAssign()
    case "*=" | ".*=" => OpTimesAssign()
    case "/=" | "./=" => OpDivideAssign()
    case "\\=" | ".\\=" => OpLeftDivideAssign()
    case "^=" | ".^=" => OpPowAssign()

    case "&=" => OpLogicalAndAssign()
    case "|=" => OpLogicalOrAssign()
    case "<<=" => OpShiftLeftAssign()
    case ">>=" => OpShiftRightAssign()

    case _ => throw new RuntimeException("Unknown assignment operator: " + op)
  }
}