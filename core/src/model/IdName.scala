package   model

import   model._
import   model.expression._
import   model.property._
import refactoring.matlab.processing.TypeInferenceProcessor
import model.statement.ExpressionStatement

object IdName {
  def apply(id: String) = new IdName(id, id)
  def apply(id: String, name: String) = new IdName(id, name)
  def unapply(n: IdName) = Some(n.id, n.name)
}

class IdName(val id: String, val name: String) extends HasProperties with Ordered[IdName]{

  def cloneName(): IdName = {
    val c = new IdName(id, name)
    c.copyProperties(this)
    c
  }
  
  def compare(that: IdName) = this.id.compare(that.id)
  
  override def equals(that: Any): Boolean = that match {
    case o: IdName => this.id == o.id
    case _ => false
  }
  
  override def hashCode: Int = id.hashCode
  
  def pretty(hash: Boolean = false): String = {
    if (hash) id else name
  }
  
  def treePretty(level: Int = 0, hash: Boolean = false): String = {
    "  " * level + "IdName: " + pretty(hash) + "\n"
  }
  def typePretty(level: Int = 0, hash: Boolean = false): String = {
    "  " * level + "IdName: " + pretty(hash) + "\n" //+ TypeInferenceProcessor.typeInference(ExpressionStatement(this)).toList.mkString("\n")
  }
}