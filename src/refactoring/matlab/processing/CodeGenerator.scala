package   refactoring.matlab.processing

import   model._
import   model.expression._
import   model.statement._
import   core.Algebra
import scala.collection.mutable

// This code generator uses templates for generating stencil codes
object CodeGenerator {
	def generate(ast: StatementBlock) = {
	  new CodeGenerator().generate(ast)
	}
}

class GeneratedCode(val main: String, val stencils: Map[String, String]) {
}

class CodeGenerator {
  
  // statements that are stencils and relevent spec
  var stencils: Map[AssignmentStatement, StencilSpec] = Map.empty
  // stores id and the type
  var typeInfo: Map[String, BasicType] = Map.empty
  // context information of current array being processed
  var currentArray: ArrayRefExpr = null
  var currentIndex: Int = 0
  
  // entry
  def generate(ast: StatementBlock) = {
    // derive type information
    typeInfo = SimpleTypeInferencer.infer(ast)
    // obtain stencil information from identified stencil statements (type info not needed)
    stencils = StencilIdentifier.identify(ast)
    
    val gen = mutable.ListBuffer.empty[String]
    gen += "#include <stdio.h>"
    gen += "#include <math.h>"
    gen += "#include \"hemi.h\""
    gen += "#include \"ndarray.h\""
    
    for (spec <- stencils.values) {
      gen += "#include \"" + spec.name + "\""
    }
    gen += " "
      
    gen += "int main() {"
    
    gen ++= generateDecl
    gen ++= generateCode(ast)
    
    gen += "}"
    
    val stencilCodes = stencils.values.map(spec => (spec.name, generateStencilCode(spec))).toMap
    val mainCode = gen.toList.mkString("\n")
    // combine statements
    new GeneratedCode(mainCode, stencilCodes)
  }
  
  protected def generateStencilCode(spec: StencilSpec): String = {
    // generate args
    // current limitations
    // - assume 1 output, many input coeff and 1 input stencil
    // - all arrays have same sizes
    // - single padding for all arrays (assume 1 radius)
    assert(spec.arraysIn.size == 1)
    val id = spec.arrayOut.owner.asInstanceOf[IdExpr].idName.id
    val t = typeInfo(id).asInstanceOf[ArrayType]
    val typeStr = generateTypeStr(t.subType)
    val coeffArrays =
      if (spec.arraysCoeff.size == 0)
        ""
      else
        spec.arraysCoeff.keys.map(id => s"const TYPE* $id").mkString("", ", ", ", ")
    val expr = generateStencilCode(spec.statement, spec)
    
    // modify template
    val tpl = StencilTemplate.JacobiTemplate
    var gen = tpl
    gen = "<<STINDEX>>".r.replaceAllIn(gen, m => spec.index.toString)
    gen = "<<STTYPE>>".r.replaceAllIn(gen, m => typeStr)
    gen = "<<STRADIUS>>".r.replaceAllIn(gen, m => spec.radius.toString)
    gen = "<<STTILEX>>".r.replaceAllIn(gen, m => spec.tileX.toString)
    gen = "<<STTILEY>>".r.replaceAllIn(gen, m => spec.tileY.toString)
    gen = "<<STREGX>>".r.replaceAllIn(gen, m => spec.regX.toString)
    gen = "<<STREGY>>".r.replaceAllIn(gen, m => spec.regX.toString)
    gen = "<<STNAME>>".r.replaceAllIn(gen, m => spec.name)
    gen = "<<STCOEFFS>>".r.replaceAllIn(gen, m => coeffArrays)
    gen = "<<STEXPRESSION>>".r.replaceAllIn(gen, m => expr)

    gen
  }
  
  protected def generateStencilCode(stmt: AssignmentStatement, spec: StencilSpec): String = {
    val rhs = generateStencilCode(stmt.rhsExpr, spec)
    s"value[ry][rx] = $rhs;"
  }
  
  protected def generateStencilCode(expr: Expr, spec: StencilSpec): String = {
    expr match {
      case e: IdExpr           => generateStencilCode(e, spec)
      case e: ConstLiteralExpr => generateStencilCode(e, spec)
      case e: UnaryExpr        => generateStencilCode(e, spec)
      case e: NAryExpr         => generateStencilCode(e, spec)
      case e: ArrayRefExpr     => generateStencilCode(e, spec) 
    }
  }
  protected def generateStencilCode(expr: IdExpr, spec: StencilSpec): String = {
    expr.pretty()
  }
  protected def generateStencilCode(expr: ConstLiteralExpr, spec: StencilSpec): String = {
    expr.pretty()
  }
  protected def generateStencilCode(expr: UnaryExpr, spec: StencilSpec): String = {
    expr.term.pretty()
  }
  protected def generateStencilCode(expr: NAryExpr, spec: StencilSpec): String = {
    expr.terms.map(t => generateStencilCode(t, spec)).mkString("(", expr.op.pretty(), ")")
  }
  protected def generateStencilCode(expr: ArrayRefExpr, spec: StencilSpec): String = {
    // calculate offsets for 3D array
    // restrictions:
    // - type of stencil is jacobi
    // - all indices are slices
    // - lower bound differences evaluate to integer
    def calcOffsets(arr: ArrayRefExpr, ref: ArrayRefExpr): List[Int] = {
      val lb1 = arr.indices.map(i => i.asInstanceOf[SliceExpr].lowerBound)
      val lb2 = ref.indices.map(i => i.asInstanceOf[SliceExpr].lowerBound)
      val diff = (lb1 zip lb2).map { case (i1, i2) =>
        Algebra.simplify(i1 - i2) match {
          case e: ConstLiteralExpr if e.isNumeric => e.numeric.toInt
          case _ => ???
        }
      }
      diff
    }
    
    val offsets = calcOffsets(expr, spec.arrayOut)
    val id = expr.owner.asInstanceOf[IdExpr].idName.id
    val gen = 
      if (spec.arraysIn contains id) {
        val dx = if (offsets(0) >= 0) s"+${offsets(0)}" else s"${offsets(0)}"
        val dy = if (offsets(1) >= 0) s"+${offsets(1)}" else s"${offsets(1)}"
        val dz = s"${math.abs(offsets(2))}"
        
        if (offsets(0) == 0 && offsets(1) == 0 && offsets(2) == 0) {
          "current[ry][rx]"
        } else if (offsets(2) == 0)	{ // no z
          s"tile[sy$dy][sx$dx]"
        } else {
          // restriction
          assert(offsets(0) == 0 && offsets(1) == 0)
          s"behind[ry][rx][$dz-1]"
        }
      } else {
        s"$id[curIndex]"
      }
    gen
  }
  
  protected def generateTypeStr(t: BasicType) = t match {
    case t: IntType    => "int"
    case t: FloatType  => "float"
    case t: DoubleType => "double"
  }

  protected def generateDecl: mutable.ListBuffer[String] = {
    val gen = mutable.ListBuffer.empty[String]
    typeInfo.foreach { case (id, iType) =>
      if (iType != null) {
        val decl = iType match {
          case t: IntType    => generateTypeStr(t) + " " + id
          case t: FloatType  => generateTypeStr(t) + " " + id
          case t: DoubleType => generateTypeStr(t) + " " + id
          case t: ArrayType  => s"NdArray<${generateTypeStr(t.subType)}> $id"
        }
        gen += decl + ";"
      }
    }
    
    gen
  }
  
  protected def generateCode(stmt: Statement): mutable.ListBuffer[String] = {
    stmt match {
      case s: StatementBlock      => generateCode(s)
      case s: AssignmentStatement => generateCode(s)
      case s: ForStatement        => generateCode(s)
      case s: ExpressionStatement => generateCode(s)
      case s @ _ => println(s); ???
    }
  }
  
  protected def generateCode(stmt: ExpressionStatement): mutable.ListBuffer[String] = {
    val gen = mutable.ListBuffer.empty[String]
    gen += generateCode(stmt.expr)
    gen
  }
  
  protected def generateTempDecl: mutable.ListBuffer[String] = {
    // TODO: liveness analysis to reduce memory requirements of temporary arrays
    // currently just have a temp for every output
    val gen = mutable.ListBuffer.empty[String]
    var temps = stencils.values.filter(spec => spec.requiresTemp).map(spec => spec.arrayOut)
    temps.foreach { array =>
      val id = array.owner.asInstanceOf[IdExpr].idName.id
      val t = typeInfo(id).asInstanceOf[ArrayType]
      val subtype = generateTypeStr(t.subType)
      val sizes = t.sizes.map(generateCode(_)).mkString(", ")
      val decl = s"NdArray<$subtype> ${id}_t = zeros<$subtype>($sizes)"
      gen += decl + ";"
    }
    gen
  }
  protected def generateCode(stmt: ForStatement): mutable.ListBuffer[String] = {
    val gen = mutable.ListBuffer.empty[String]
    val init = generateCode(stmt.initStmt)(0).toString().drop(1).dropRight(1)	// remove brackets
    val cond = generateCode(stmt.condExpr.get)
    val iter = generateCode(stmt.iterExpr.get)
    
    if (stmt.isStencil) {
      // initialize the temp vars
      gen ++= generateTempDecl
    }
    // TODO obtain domains and adjust to base 0
    gen += s"for (int $init; $cond; $iter) {"
    gen ++= generateCode(stmt.body)
    gen += "}"
    gen
  }
  
  protected def generateCode(stmt: StatementBlock): mutable.ListBuffer[String] = {
    val gen = mutable.ListBuffer.empty[String]
    stmt.statements.foldLeft(gen) {
      case (res, st) =>
        res ++= generateCode(st)
    }
    gen
  }
  
  protected def generateStencilCall(stmt: AssignmentStatement): mutable.ListBuffer[String] = {
    def generateArrayArg = (arg: ArrayRefExpr, read: Boolean) => {
      val id = arg.owner.asInstanceOf[IdExpr].idName.id
      val aType = typeInfo(id).asInstanceOf[ArrayType]
      if (read)
        s"$id.getHemi()->readOnlyDevicePtr()"
      else
        s"$id.getHemi()->writeOnlyDevicePtr()"
    }
    
    val gen = mutable.ListBuffer.empty[String]
    val spec = stencils(stmt)

    val arraysIn = spec.arraysIn.values.toList.map(generateArrayArg(_, true)).mkString(", ")
    val arraysCoeff = spec.arraysCoeff.values.toList.map(generateArrayArg(_, true)).mkString(", ")
    val arrayOut = s"${spec.outName}.getHemi()->writeOnlyDevicePtr()"
    val aType = typeInfo(spec.arrayOut.owner.asInstanceOf[IdExpr].idName.id).asInstanceOf[ArrayType]
    val sizeStr = aType.sizes.map(generateCode(_)).mkString(", ")
    val dimX = generateCode(aType.sizes(0))
    val dimY = generateCode(aType.sizes(1))
    val tileX = s"TILEX${spec.index}"
    val tileY = s"TILEY${spec.index}"
    val regX = s"REGX${spec.index}"
    val regY = s"REGX${spec.index}"
    val gridDim = s"calcGridDim($dimX, $dimY, $tileX, $tileY, $regX, $regY)"
    val blkDim = s"dim3($tileX, $tileY)"
    if (arraysCoeff == "")
      gen += s"HEMI_KERNEL_LAUNCH(${spec.name}, $gridDim, $blkDim, 0, 0, $arrayOut, $arraysIn, $sizeStr);"
    else
      gen += s"HEMI_KERNEL_LAUNCH(${spec.name}, $gridDim, $blkDim, 0, 0, $arrayOut, $arraysIn, $arraysCoeff, $sizeStr);"
    if (spec.requiresTemp) {
      val outId = spec.arrayOut.owner.asInstanceOf[IdExpr].idName.id
      gen += s"${spec.outName}.swap($outId);"
      //gen += s"swap(${spec.outName}, ${generateArrayArg(spec.arrayOut)});"
    }
    gen
  }
  
  protected def generateCode(stmt: AssignmentStatement): mutable.ListBuffer[String] = {
    val gen = mutable.ListBuffer.empty[String]
    
    val isStencil = stencils contains stmt
    if (isStencil) {
      //gen += "// stencil\n" + stmt.pretty()
      gen ++= generateStencilCall(stmt)
    } else {
      stmt.lhsExpr match {
        case e: IdExpr =>
          // single line expression
          val lhs = generateCode(stmt.lhsExpr)
          val rhs = generateCode(stmt.rhsExpr)
          gen += s"$lhs = $rhs;"      
        case e: ArrayRefExpr =>
          // array access expression
          val indices = e.indices.zipWithIndex

          indices foreach { case (ind, i) =>
            if (ind.isInstanceOf[SliceExpr]) {
              currentArray = e
              val slice = ind.asInstanceOf[SliceExpr]
              val iv = "i" + i
              val lb = generateCode(slice.lowerBound)+"-1"
              val ub = generateCode(slice.upperBound)
              val inc = generateCode(slice.stride)
              currentArray = null
              gen += s"for (int $iv = $lb; $iv < $ub; $iv += $inc) {"
            }
          }
          val lhs = generateCode(stmt.lhsExpr)
          val rhs = generateCode(stmt.rhsExpr)
          gen += s"$lhs = $rhs;"      
          indices foreach { case (ind, i) =>
            if (ind.isInstanceOf[SliceExpr]) gen += "}"
          }
      }
    }
    
    gen
  }
  
  ///////////////////////////// Expression generator //////////////////////////
  protected def generateCode(expr: Expr): String = {
    val gen = 
    expr match {
      case e: IdExpr           => generateCode(e)
      case e: ConstLiteralExpr => generateCode(e)
      case e: UnaryExpr        => generateCode(e)
      case e: NAryExpr         => generateCode(e)
      case e: FunctionCallExpr => generateCode(e)
      case e: ArrayRefExpr         => generateCode(e); 
      case e: ArrayEndExpr         => generateCode(e)
      case e: ArrayCompositionExpr => "[ArrayCompositionExpr]"
      case e: SliceExpr            => generateCode(e)
    }
    gen
  }
  
  protected def generateCode(expr: IdExpr): String = {
    expr.pretty()
  }
  protected def generateCode(expr: ConstLiteralExpr): String = {
    expr.pretty()
  }
  protected def generateCode(expr: UnaryExpr): String = {
    expr.term.pretty()
  }
  protected def generateCode(expr: NAryExpr): String = {
    expr.terms.map(t => generateCode(t)).mkString("(", expr.op.pretty(), ")")
  }
  protected def generateCode(expr: FunctionCallExpr): String = {
    val func = generateCode(expr.funcNameExpr)
    val params = expr.params.map(i => generateCode(i)).mkString(",")
    func match {
      case "zeros" | "ones" =>
        // TODO: now default to float
        s"$func<float>($params)"
      case _ => s"$func($params)"
    }
    
  }
  protected def generateCode(expr: ArrayRefExpr): String = {
    currentArray = expr
    val id = generateCode(expr.owner)
    val aType = typeInfo(id).asInstanceOf[ArrayType]
  
    currentIndex = 0
    var indStr = expr.indices.map {i =>
      val c = generateCode(i)
      currentIndex += 1
      c
    }
//    // flatten into (i0*x+i1) form    
//    var indStr = ""
//    currentIndex = 0
//    for (ind <- expr.indices) {
//      if (currentIndex == 0) {
//        indStr = generateCode(ind)
//      } else {
//        val dim = generateCode(aType.sizes(currentIndex-1))
//        indStr = s"($indStr*$dim+${generateCode(ind)})"
//      }
//      currentIndex += 1
//    }
     
    currentArray = null
    currentIndex = 0
    //id + "[" + indStr + "]"
    id + "(" + indStr.mkString(", ") + ")"
  }
  protected def generateCode(expr: ArrayEndExpr): String = {
    if (currentArray != null) {
      val id = currentArray.owner.asInstanceOf[IdExpr].idName.id
      val aType = typeInfo(id).asInstanceOf[ArrayType]
      generateCode(aType.sizes(currentIndex))
    } else {
      "[end:syntax error]"
    }
  }
  protected def generateCode(expr: SliceExpr): String = {
//    if (currentIndex != currentArray.rank-1) {
//      val id = currentArray.owner.asInstanceOf[IdExpr].idName.id
//      val aType = typeInfo(id).asInstanceOf[ArrayType]
//      s"i${currentIndex}*" + generateCode(aType.sizes(currentIndex))
//    } else {
//      s"i${currentIndex}"
//    }
    s"i${currentIndex}"
  }

}