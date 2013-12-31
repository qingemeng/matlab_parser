package   model

import   model.statement._
import   model.expression._
import   model.property._

// TranslationUnit represents a source code file
// fileName: full path of source file
// functions: functions defined in source file
case class TranslationUnit(fileName: String, functions: List[FunctionDef]) 
  extends HasProperties {
  
  def cloneTU(): TranslationUnit = {
    val c = new TranslationUnit(fileName, functions.map(func => func.cloneFunc()))
    c.copyProperties(this)
    c
  }
  
  def pretty(level: Int = 0): String = {
    val str = new StringBuilder
    str.append(String.format("TranslationUnit: %s\n", fileName))
    for (f <- functions)
      str.append(f.pretty(level + 1))
    str.append("\n")
    str.toString
  }
  
  def treePretty(level: Int = 0): String = {
    val str = new StringBuilder
    str.append(String.format("TranslationUnit: %s\n", fileName))
    for (f <- functions)
      str.append(f.treePretty(level + 1))
    str.append("\n")
    str.toString
  }
}