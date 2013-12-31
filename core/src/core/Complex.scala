package   core

sealed abstract class Complex[T: Fractional](val real: T, val imag: T) {
  import scala.math.Fractional.Implicits._
  import scala.math.Ordering.Implicits._
  import scala.math._

  
  def conj: T = this.real - this.imag
  def norm: T = this.real * this.real + this.imag * this.imag
  def abs: Double = sqrt(norm.toDouble)
  def arg: Double =  atan2(this.imag.toDouble, this.real.toDouble)
  
  def plus(that: Complex[T]) = (this.real + that.real, this.imag + that.imag)
  def minus(that: Complex[T]) = (this.real - that.real, this.imag - that.imag)
  def times(that: Complex[T]) = {
    val part1 = this.real * that.real
    val part2 = that.imag * that.imag
    (part1 - part2, part1 + part2)
  }
  def negate() = (-this.real, -this.imag)
  
  def div(that: Complex[T]) = {
    val denom = that.real * that.real + that.imag * that.imag
    val newReal = (this.real * that.real + this.imag * that.imag) / denom
    val newImag = (this.imag * that.real - this.real * that.imag) / denom
    
    (newReal, newImag)
  }
  
  override def equals(that: Any): Boolean = that match {
    case x: ComplexFloat => this.real == x.real && this.imag == x.imag
    case x: ComplexDouble => this.real == x.real && this.imag == x.imag
    case (r, i) => this.real == r && this.imag == i
    case _ => false
  }   
  
  override def toString(): String = real.toString + (if (imag.toDouble() < 0) "-" + (-imag) else "+" + imag) + "i"
}


object ComplexFloat {
  implicit def FloatBox(value: Float) = ComplexFloat(value)
  implicit def DoubleBox(value: Double) = ComplexFloat(value.toFloat)
  
  implicit def TupleIntBox(value: (Int, Int)) =  ComplexFloat(value._1, value._2)
  implicit def TupleLongBox(value: (Long, Long)) =  ComplexFloat(value._1, value._2)
  implicit def TupleFloatBox(value: (Float, Float)) = ComplexFloat(value._1, value._2)
  implicit def TupleDoubleBox(value: (Double, Double)) = ComplexFloat(value._1.toFloat, value._2.toFloat)
  
  implicit def ComplexDouble2Float(value: ComplexDouble) = ComplexFloat(value.real.toFloat, value.imag.toFloat)
  
  def apply(value: Float) = new ComplexFloat(value, 0)
  def apply(real: Float, imag: Float) = new ComplexFloat(real, imag)
  
  def unapply(complex: ComplexFloat) = Some(complex.real, complex.imag)
}

class ComplexFloat(real: Float, imag: Float) extends Complex[Float](real, imag) with Ordered[ComplexFloat]{
  def +(that: ComplexFloat):ComplexFloat = plus(that)
  def -(that: ComplexFloat):ComplexFloat = minus(that)
  def *(that: ComplexFloat):ComplexFloat = times(that)
  def /(that: ComplexFloat):ComplexFloat = div(that)
  
  def unary_-():ComplexFloat = negate()
  
  override def compare(that: ComplexFloat): Int = {
    val result = this.real compare that.real
    if(result == 0){
      this.imag compare that.imag
    } else {
      result
    }
  }
}

object ComplexDouble {
  implicit def DoubleBox(value: Double) = ComplexDouble(value)
  
  implicit def TupleIntBox(value: (Int, Int)) = ComplexDouble(value._1, value._2)
  implicit def TupleLongBox(value: (Long, Long)) = ComplexDouble(value._1, value._2)
  implicit def TupleFloatBox(value: (Float, Float)) = ComplexDouble(value._1, value._2)
  implicit def TupleDoubleBox(value: (Double, Double)) = ComplexDouble(value._1, value._2)
  
  implicit def ComplexFloat2Double(value: ComplexFloat) = ComplexDouble(value.real, value.imag)
  
  def apply(value: Double) = new ComplexDouble(value, 0)
  def apply(real: Double, imag: Double) = new ComplexDouble(real, imag)
  
  def unapply(complex: ComplexDouble) = Some(complex.real, complex.imag)
}

class ComplexDouble(real: Double, imag: Double) extends Complex[Double](real, imag) with Ordered[ComplexDouble]{
  def +(that: ComplexDouble):ComplexDouble = plus(that)
  def -(that: ComplexDouble):ComplexDouble = minus(that)
  def *(that: ComplexDouble):ComplexDouble = times(that)
  def /(that: ComplexDouble):ComplexDouble = div(that)
  def unary_-():ComplexDouble = negate()
  
  override def compare(that: ComplexDouble): Int = {
    val result = this.real compare that.real
    if(result == 0){
      this.imag compare that.imag
    } else {
      result
    }
  }
}