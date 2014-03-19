package model.expression

object ArrayEndExpr {
  def apply() = new ArrayEndExpr(None)
  def apply(owner: Expr) = new ArrayEndExpr(Some(owner))
  def apply(owner: Option[Expr]) = new ArrayEndExpr(owner)
  def unapply(e: ArrayEndExpr): Option[Expr] = e.owner
}

//Represents the end of the array
class ArrayEndExpr(private var _owner: Option[Expr]) extends Expr {
  update(_owner)
  
  def owner = _owner  
  def update(owner: Option[Expr] = _owner) = {
    owner match {
      case Some(o) => {
        _owner = owner
        o.setParent(this)
      }
      case None => _owner = None
    }
    this 
  }
  
  def cloneExpr(): Expr = {
    val c = owner match {
      case Some(o) => ArrayEndExpr(o.cloneExpr)
      case None => ArrayEndExpr()
    }

    c.base_copyFrom(this)
    c
  }

  override def equals(that: Any): Boolean = that match {
    case o: ArrayEndExpr => this.owner.equals(o.owner)
    case _ => false
  }

  override def pretty(hash: Boolean = false): String = {
    owner match {
      case Some(o) => "endof(" + o.pretty(hash) + ")"
      case None => "end"
    }
  }
  
  override def treePretty(level: Int = 0, hash: Boolean = false): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    
    str.append("ArrayEndExpr: ")
    str.append(pretty(hash))
    str.append("\n")
    
    owner match {
      case Some(o) => str.append(o.treePretty(level+1, hash))
      case None =>
    }
    
    str.toString
  }
  //TODO:gm,rewrite
  override def semanticAnalyse(level: Int = 0, hash: Boolean = false): String = {
//    val str = new StringBuilder
//    str.append(indentStr(level))
//    str.append("\n")
//    str.toString
val str = new StringBuilder
    str.append(indentStr(level))

    str.append("ArrayEndExpr: ")
//    str.append(pretty(hash))
    str.append("\n")

    owner match {
      case Some(o) => str.append(o.semanticAnalyse(level+1, hash))
      case None =>
    }

    str.toString
  }
}