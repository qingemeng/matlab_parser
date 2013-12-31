package   model

import   model.statement._
import   model.expression._
import   model.property._

object FunctionDef {
  def apply(idName: IdName, params: List[IdName], body: Statement):FunctionDef = FunctionDef(idName, params, List(), List(), body, None)
  def apply(idName: IdName, params: List[IdName], inputs: List[IdName], outputs: List[IdName], body: Statement):FunctionDef = FunctionDef(idName, params, inputs, outputs, body, None)
}

case class FunctionDef(idName: IdName, params: List[IdName], var inputs: List[IdName], var outputs: List[IdName], var body: Statement, retType: Option[BasicType]) extends HasProperties {
  def cloneFunc(): FunctionDef = {
    val newFunc = FunctionDef(
      idName.cloneName,
      params.map(param => param.cloneName),
      inputs.map(input => input.cloneName),
      outputs.map(output => output.cloneName),
      body.cloneStmt,
      retType
    )
    
    newFunc.copyProperties(this)
    newFunc
  }
  
  def pretty(level: Int = 0): String = {
    val str = new StringBuilder
    if(!inputs.isEmpty || !outputs.isEmpty){
      str.append(String.format("(%s) = %s (%s)\n", 
          outputs.map(t => t.pretty(false)).mkString(", "),
          idName.pretty(false), 
          inputs.map(t => t.pretty(false)).mkString(", ")
      ))
    } else {
      val ret = retType match {
        case Some(t) => t.toString()
        case None => "void"
      }
      str.append(String.format("%s %s (%s)\n", ret, idName.pretty(false), params.map(_.pretty(false)).mkString(", ")))
    }
    str.append(body.pretty(level + 1))
    str.append("\n")
    str.toString
  }
  
  def treePretty(level: Int = 0): String = {
    def indentStr(level: Int): String = "  " * level
    
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("Function:\n")
    
    str.append(indentStr(level))
    str.append("->Parameters: \n")
    params.foreach(parm => {
      str.append(parm.treePretty(level+2, false))
    })
    
    str.append(indentStr(level))
    str.append("->FunctionBody: \n")    
    str.append(body.treePretty(level + 2))
    
    str.toString
  }
}
