package model

import  model.expression.Expr

object ArrayInfo {
  def apply(arrayType: BasicType, rank: Int, sizes: List[Expr] = List.empty) = new ArrayInfo(arrayType, rank, sizes)
}

// each size is null if dynamic
class ArrayInfo(val arrayType: BasicType, val rank: Int, val sizes: List[Expr]){
  def hasUnknownSize(): Boolean = sizes.isEmpty || sizes.contains(null)
  
  def toArrayType: ArrayType = ArrayType(arrayType, rank, sizes)
  
  // deep clone of array info
  def cloneArrayInfo(): ArrayInfo = {
    new ArrayInfo(arrayType, rank, sizes.map(size => size.cloneExpr()))
  }
  
  def pretty(hash:Boolean = false): String = {
    arrayType + sizes.map(size => "[" + size.pretty(hash) + "]").mkString
  }


  def treePretty(level: Int = 0, hash:Boolean = false): String = {
    def indentStr(level: Int): String = "  " * level

    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("ArrayInfo: ")
    str.append(pretty(hash))
    str.append("\n")

    str.append(indentStr(level))
    str.append("->Type: ")
    str.append(arrayType)
    str.append("\n")

    str.append(indentStr(level))
    str.append("->Sizes:\n")
    sizes.foreach(size => str.append(size.treePretty(level+2, hash)))
    str.toString
  }
  def semanticAnalyse(level: Int = 0, hash:Boolean = false): String = {
    def indentStr(level: Int): String = "  " * level

    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("ArrayInfo: ")
//    str.append(pretty(hash))
    str.append("\n")

    str.append(indentStr(level))
    str.append("->Type: ")
    str.append(arrayType)
    str.append("\n")

    str.append(indentStr(level))
    str.append("->Sizes:\n")
    sizes.foreach(size => str.append(size.semanticAnalyse(level+2, hash)))
    str.toString
  }
  
  override def toString(): String = pretty()
}