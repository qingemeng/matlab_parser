package model.property

import scala.collection.mutable.Map

//Based on http://stackoverflow.com/questions/7335946/class-type-as-key-in-map-in-scala

class PropertyMap private(val inner: Map[Manifest[_], Property]) {
  def +[A : Manifest](a: Property) ={
    inner += (manifest[A] -> a)
    this
  }
  
  def -[A : Manifest]() = {
    inner -= manifest[A]
    this
  }
  
  def apply[A : Manifest]: A = {
    if(inner.keySet.contains(manifest[A])){
      inner(manifest[A]).asInstanceOf[A]
    } else {
      val value = manifest[A].erasure.newInstance().asInstanceOf[Property]
      inner += (manifest[A] -> value)
      value.asInstanceOf[A]
    }
  }
  
  def get[A : Manifest]: Option[A] = inner.get(manifest[A]).map(_.asInstanceOf[A])
  
  def copy(src: PropertyMap){
    src.inner.foreach{ case (key, value) => {
      inner += (key -> value.cloneProperty())
    }}
  }
  
  override def toString = inner.toString
  override def equals(other: Any) = other match {
    case that: PropertyMap => this.inner == that.inner
    case _ => false
  }
  override def hashCode = inner.hashCode
}

object PropertyMap {
  def apply() = new PropertyMap(scala.collection.mutable.Map.empty)
}