package   model


import core._
object NumericValue {
  implicit def CharBox(value: Char) =  new NumericValue(value)
  implicit def ByteBox(value: Byte) =  new NumericValue(value)
  implicit def ShortBox(value: Short) =  new NumericValue(value)
  implicit def IntBox(value: Int) =  new NumericValue(value)
  implicit def LongBox(value: Long) =  new NumericValue(value)
  implicit def FloatBox(value: Float) =  new NumericValue(value)
  implicit def DoubleBox(value: Double) =  new NumericValue(value)
  implicit def ComplexFloatBox(value: ComplexFloat) =  new NumericValue(value)
  implicit def ComplexDoubleBox(value: ComplexDouble) =  new NumericValue(value)
  
  def apply(value: Any) = value match {
    case x: Char => new NumericValue(x)
    case x: Byte =>  new NumericValue(x)
    case x: Short => new NumericValue(x)
    case x: Int => new NumericValue(x)
    case x: Long => new NumericValue(x)
    case x: Float => new NumericValue(x)
    case x: Double => new NumericValue(x)
    case x: ComplexFloat => new NumericValue(x)
    case x: ComplexDouble => new NumericValue(x)
    case _ => throw new UnsupportedOperationException()
  }
  
  def apply() = new NumericValue(0)
  
  def unapply(n: NumericValue) = n.value match {
    case x: Char => Some(x)
    case x: Byte =>  Some(x)
    case x: Short => Some(x)
    case x: Int => Some(x)
    case x: Long => Some(x)
    case x: Float => Some(x)
    case x: Double => Some(x)
    case x: ComplexFloat => Some(x)
    case x: ComplexDouble => Some(x)
    case _ => None
  }
}

class NumericValue(val value: Any) extends Ordered[NumericValue]{
  import TypeUtil._
  
  def +(that: NumericValue): NumericValue = {
    val code = maxCode(value, that.value)

    val newValue = code match {
      case CHAR => this.toChar + that.toChar
      case BYTE => this.toInt + that.toInt
      case SHORT => this.toShort + that.toShort
      case INT => this.toInt + that.toInt
      case LONG => this.toLong + that.toLong
      case FLOAT => this.toFloat + that.toFloat
      case DOUBLE => this.toDouble + that.toDouble
      case COMPLEX_FLOAT => this.toComplexFloat + that.toComplexFloat
      case COMPLEX_DOUBLE => this.toComplexDouble + that.toComplexDouble
      case _ => throw new UnsupportedOperationException()
    }
    NumericValue(newValue)
  }

  def -(that: NumericValue): NumericValue = {
    val code = maxCode(value, that.value)

    val newValue = code match {
      case CHAR => this.toChar - that.toChar
      case BYTE => this.toInt - that.toInt
      case SHORT => this.toShort - that.toShort
      case INT => this.toInt - that.toInt
      case LONG => this.toLong - that.toLong
      case FLOAT => this.toFloat - that.toFloat
      case DOUBLE => this.toDouble - that.toDouble
      case COMPLEX_FLOAT => this.toComplexFloat - that.toComplexFloat
      case COMPLEX_DOUBLE => this.toComplexDouble - that.toComplexDouble
      case _ => throw new UnsupportedOperationException()
    }
    NumericValue(newValue)
  }

  def *(that: NumericValue): NumericValue = {
    val code = maxCode(value, that.value)

    val newValue = code match {
      case CHAR => this.toChar * that.toChar
      case BYTE => this.toInt * that.toInt
      case SHORT => this.toShort * that.toShort
      case INT => this.toInt * that.toInt
      case LONG => this.toLong * that.toLong
      case FLOAT => this.toFloat * that.toFloat
      case DOUBLE => this.toDouble * that.toDouble
      case COMPLEX_FLOAT => this.toComplexFloat * that.toComplexFloat
      case COMPLEX_DOUBLE => this.toComplexDouble * that.toComplexDouble
      case _ => throw new UnsupportedOperationException()
    }
    NumericValue(newValue)
  }
  
  def unary_-(): NumericValue = {
    val newValue = value match {
      case x: Char => -x
      case x: Byte => -x
      case x: Short => -x
      case x: Int => -x
      case x: Long => -x
      case x: Float => -x
      case x: Double => -x
      case x: ComplexFloat => -x
      case x: ComplexDouble => -x
      case _ => throw new UnsupportedOperationException()
    }
    NumericValue(newValue)
  }
  
  override def compare(that: NumericValue) = {
    val code = maxCode(value, that.value)

    code match {
      case CHAR => this.toChar compare that.toChar
      case BYTE => this.toInt compare that.toInt
      case SHORT => this.toShort compare that.toShort
      case INT => this.toInt compare that.toInt
      case LONG => this.toLong compare that.toLong
      case FLOAT => this.toFloat compare that.toFloat
      case DOUBLE => this.toDouble compare that.toDouble
      case COMPLEX_FLOAT => this.toComplexFloat compare that.toComplexFloat
      case COMPLEX_DOUBLE => this.toComplexDouble compare that.toComplexDouble
      case _ => throw new UnsupportedOperationException()
    }
  }

  override def equals(that: Any): Boolean = {
    def equals(that: NumericValue): Boolean = compare(that) == 0
    
    that match {
      case x: NumericValue => equals(x)
      case x: java.lang.Character => equals(x.charValue())
      case x: java.lang.Byte => equals(x.byteValue())
      case x: java.lang.Short => equals(x.shortValue())
      case x: java.lang.Integer => equals(x.intValue()) 
      case x: java.lang.Long => equals(x.longValue())
      case x: java.lang.Float => equals(x.floatValue())
      case x: java.lang.Double => equals(x.doubleValue())
      case x: ComplexFloat => equals(x)
      case x: ComplexDouble => equals(x)
      case _ => false
    }
  } 
  
  override def toString(): String = value.toString()
  
  /********************************************************************************/
  def toChar():Char = value match {
    case x: Char => x.toChar
    case x: Byte => x.toChar
    case x: Short => x.toChar
    case x: Int => x.toChar
    case x: Long => x.toChar
    case x: Float => x.toChar
    case x: Double => x.toChar
  }
  
  def toByte():Byte = value match {
    case x: Char => x.toByte
    case x: Byte => x.toByte
    case x: Short => x.toByte
    case x: Int => x.toByte
    case x: Long => x.toByte
    case x: Float => x.toByte
    case x: Double => x.toByte
  }

  def toShort():Short = value match {
    case x: Char => x.toShort
    case x: Byte => x.toShort
    case x: Short => x.toShort
    case x: Int => x.toShort
    case x: Long => x.toShort
    case x: Float => x.toShort
    case x: Double => x.toShort
  }
  
  def toInt():Int = value match {
    case x: Char => x.toInt
    case x: Byte => x.toInt
    case x: Short => x.toInt
    case x: Int => x.toInt
    case x: Long => x.toInt
    case x: Float => x.toInt
    case x: Double => x.toInt
  }
  
  def toLong():Long = value match {
    case x: Char => x.toLong
    case x: Byte => x.toLong
    case x: Short => x.toLong
    case x: Int => x.toLong
    case x: Long => x.toLong
    case x: Float => x.toLong
    case x: Double => x.toLong
  }
  
  def toFloat():Float = value match {
    case x: Char => x.toFloat
    case x: Byte => x.toFloat
    case x: Short => x.toFloat
    case x: Int => x.toFloat
    case x: Long => x.toFloat
    case x: Float => x.toFloat
    case x: Double => x.toFloat
  }
  
  def toDouble():Double = value match {
    case x: Char => x.toDouble
    case x: Byte => x.toDouble
    case x: Short => x.toDouble
    case x: Int => x.toDouble
    case x: Long => x.toDouble
    case x: Float => x.toDouble
    case x: Double => x.toDouble
  }
  
  def toComplexFloat(): ComplexFloat = value match {
    case x: Char => x
    case x: Byte => x
    case x: Short => x
    case x: Int => x
    case x: Long => x
    case x: Float => x
    case x: Double => x.toFloat
    case x: ComplexFloat => x
  }
  
  def toComplexDouble(): ComplexDouble = value match {
    case x: Char => x
    case x: Byte => x
    case x: Short => x
    case x: Int => x
    case x: Long => x
    case x: Float => x
    case x: Double => x
    case x: ComplexFloat => x
    case x: ComplexDouble => x
  }
}