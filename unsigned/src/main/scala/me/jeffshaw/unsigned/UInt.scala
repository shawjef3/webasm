package me.jeffshaw.unsigned

import scala.collection.mutable.ArrayBuffer

case class UInt(private[unsigned] val i: Int) {
  def toByte: Byte = i.toByte
  def toShort: Short = i.toShort
  def toChar: Char = i.toChar
  def toInt: Int = i
  def toLong: Long = i & 0xffffffffL
  def toULong: ULong = ULong(
    if (i < 0)
      toLong
    else toLong + 0x80000000L
  )

  def toFloat: Float = toLong.toFloat

  def toDouble: Double = toLong.toDouble

  def unary_~ = UInt(~i)

  def <<(x: Int): UInt =
    UInt(i << x)

  def <<(x: Long): UInt =
    UInt(i << x)

  def >>(x: Int): UInt =
    UInt(i >>> x)

  def >>(x: Long): UInt =
    UInt(i >>> x)

  def ==(x: Byte): Boolean =
    i == x

  def ==(x: Short): Boolean =
    i == x

  def ==(x: Char): Boolean =
    i == x

  def ==(x: Float): Boolean =
    toFloat == x

  def ==(x: Double): Boolean =
    toDouble == x

  def ==(x: Int): Boolean = i == x
  def ==(x: Long): Boolean = i == x
  def ==(x: UInt): Boolean = i == x.i
  def ==(x: ULong): Boolean = i == x.i

  def !=(x: Byte): Boolean = i != x
  def !=(x: Short): Boolean = i != x
  def !=(x: Char): Boolean = i != x
  def !=(x: Float): Boolean = i != x
  def !=(x: Double): Boolean = i != x
  def !=(x: Int): Boolean = i != x
  def !=(x: Long): Boolean = i != x
  def !=(x: UInt): Boolean = i != x.i
  def !=(x: ULong): Boolean = i != x.i

  def <(x: Byte): Boolean = i < x
  def <(x: Short): Boolean = i < x
  def <(x: Char): Boolean = i < x
  def <(x: Float): Boolean = i < x
  def <(x: Double): Boolean = i < x
  def <(x: Int): Boolean = java.lang.Integer.compareUnsigned(i, x) < 0
  def <(x: Long): Boolean = toLong < x
  def <(x: UInt): Boolean = java.lang.Integer.compareUnsigned(i, x.i) < 0
  def <(x: ULong): Boolean = toULong < x.i

  def <=(x: Byte): Boolean = i <= x
  def <=(x: Short): Boolean = i <= x
  def <=(x: Char): Boolean = i <= x
  def <=(x: Float): Boolean = i <= x
  def <=(x: Double): Boolean = i <= x
  def <=(x: Int): Boolean = java.lang.Integer.compareUnsigned(i, x) <= 0
  def <=(x: Long): Boolean = toLong <= x
  def <=(x: UInt): Boolean = java.lang.Integer.compareUnsigned(i, x.i) <= 0
  def <=(x: ULong): Boolean = toULong <= x

  def >(x: Byte): Boolean = i > x
  def >(x: Short): Boolean = i > x
  def >(x: Char): Boolean = i > x
  def >(x: Float): Boolean = i > x
  def >(x: Double): Boolean = i > x
  def >(x: Int): Boolean = java.lang.Integer.compareUnsigned(i, x) > 0
  def >(x: Long): Boolean = toLong > x
  def >(x: UInt): Boolean = java.lang.Integer.compareUnsigned(i, x.i) > 0
  def >(x: ULong): Boolean = toULong > x

  def >=(x: Byte): Boolean = i >= x
  def >=(x: Short): Boolean = i >= x
  def >=(x: Char): Boolean = i >= x
  def >=(x: Float): Boolean = toFloat >= x
  def >=(x: Double): Boolean = toDouble >= x
  def >=(x: Int): Boolean = java.lang.Integer.compareUnsigned(i, x) >= 0
  def >=(x: Long): Boolean = toLong >= x
  def >=(x: UInt): Boolean = java.lang.Integer.compareUnsigned(i, x.i) >= 0
  def >=(x: ULong): Boolean = toULong >= x
  
  def |(x: Byte): UInt = UInt(i | x)
  def |(x: Short): UInt = UInt(i | x)
  def |(x: Char): UInt = UInt(i | x)
  def |(x: Int): UInt = UInt(i | x)
  def |(x: Long): Long = toLong | x
  def |(x: UInt): UInt = UInt(i | x.i)
  def |(x: ULong): ULong = toULong | x
  
  def &(x: Byte): UInt = UInt(i & x)
  def &(x: Short): UInt = UInt(i & x)
  def &(x: Char): UInt = UInt(i & x)
  def &(x: Int): UInt = UInt(i & x)
  def &(x: Long): Long = toLong & x
  def &(x: UInt): UInt = UInt(i & x.i)
  def &(x: ULong): ULong = toULong & x

  def ^(x: Byte): UInt = UInt(i ^ x)
  def ^(x: Short): UInt = UInt(i ^ x)
  def ^(x: Char): UInt = UInt(i ^ x)
  def ^(x: Int): UInt = UInt(i ^ x)
  def ^(x: Long): Long = toLong ^ x
  def ^(x: UInt): UInt = UInt(i ^ x.i)
  def ^(x: ULong): ULong = toULong ^ x

  def +(x: Byte): UInt = UInt(i + x)
  def +(x: Short): UInt = UInt(i + x)
  def +(x: Char): UInt = UInt(i + x)
  def +(x: Float): Float = toFloat + x
  def +(x: Double): Double = toDouble + x
  def +(x: Int): UInt = UInt(i + x)
  def +(x: Long): Long = toLong + x
  def +(x: UInt): UInt = UInt(i + x.i)
  def +(x: ULong): ULong = toULong + x

  def -(x: Byte): UInt = UInt(i - x)
  def -(x: Short): UInt = UInt(i - x)
  def -(x: Char): UInt = UInt(i - x)
  def -(x: Float): Float = toFloat - x
  def -(x: Double): Double = toDouble - x
  def -(x: Int): UInt = UInt(i - x)
  def -(x: Long): Long = toLong - x
  def -(x: UInt): UInt = UInt(i - x.i)
  def -(x: ULong): ULong = toULong - x

  def *(x: Byte): UInt = UInt(i * x)
  def *(x: Short): UInt = UInt(i * x)
  def *(x: Char): UInt = UInt(i * x)
  def *(x: Float): Float = toFloat * x
  def *(x: Double): Double = toDouble * x
  def *(x: Int): UInt = UInt(i * x)
  def *(x: Long): Long = toLong * x
  def *(x: UInt): UInt = UInt(i * x.i)
  def *(x: ULong): ULong = toULong * x

  def /(x: Byte): UInt = UInt(i / x)
  def /(x: Short): UInt = UInt(i / x)
  def /(x: Char): UInt = UInt(i / x)
  def /(x: Float): Float = toFloat / x
  def /(x: Double): Double = toDouble / x
  def /(x: Int): UInt = UInt(java.lang.Integer.divideUnsigned(i, x))
  def /(x: Long): Long = toLong / x
  def /(x: UInt): UInt = UInt(java.lang.Integer.divideUnsigned(i, x.i))
  def /(x: ULong): ULong = toULong / x

  def %(x: Byte): UInt = UInt(i % x)
  def %(x: Short): UInt = UInt(i % x)
  def %(x: Char): UInt = UInt(i % x)
  def %(x: Float): Float = toFloat % x
  def %(x: Double): Double = toDouble % x
  def %(x: Int): UInt = UInt(java.lang.Integer.remainderUnsigned(i, x))
  def %(x: Long): Long = toLong % x
  def %(x: UInt): UInt = UInt(java.lang.Integer.remainderUnsigned(i, x.i))
  def %(x: ULong): ULong = toULong % x

  override def toString: String =
    java.lang.Integer.toUnsignedString(i)

}

object UInt {

  final val MinValue =
    UInt(0)

  final val MaxValue =
    UInt(-1)

  def valueOf(x: String, radix: Int = 10): UInt =
    UInt(java.lang.Integer.parseUnsignedInt(x, radix))

  implicit def uint2float(x: UInt): Float = x.toFloat

  implicit def uint2double(x: UInt): Double = x.toDouble

  //avoids boxing
  class Buffer(vals: ArrayBuffer[Int]) extends scala.collection.mutable.Buffer[UInt]() {

    override def update(n: Int, newelem: UInt): Unit = vals.update(n, newelem.i)

    override def +=(elem: UInt): this.type = {
      vals += elem.i
      this
    }

    override def clear(): Unit = vals.clear()

    override def +=:(elem: UInt): this.type = {
      elem.i +=: vals
      this
    }

    override def insertAll(n: Int, elems: Traversable[UInt]): Unit = vals.insertAll(n, elems.map(_.i))

    override def remove(n: Int): UInt = {
      UInt(vals.remove(n))
    }

    override def apply(idx: Int): UInt = UInt(vals(idx))

    override def length: Int = vals.length

    override def iterator: Iterator[UInt] = {
      val underlying = vals.iterator
      new Iterator[UInt] {
        override def hasNext: Boolean = underlying.hasNext

        override def next(): UInt = UInt(underlying.next())
      }
    }

  }

  object Buffer {
    def apply(vals: UInt*): Buffer =
      new Buffer(ArrayBuffer(vals.map(_.i): _*))

    def empty: Buffer = new Buffer(ArrayBuffer.empty)
  }

}
