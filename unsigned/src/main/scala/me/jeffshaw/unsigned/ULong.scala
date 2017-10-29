package me.jeffshaw.unsigned

import scala.collection.mutable.ArrayBuffer

case class ULong(private[unsigned] val i: Long) {
  private def unsignedPart =
    i & ULong.unsignedMask

  def toByte: Byte = i.toByte
  def toShort: Short = i.toShort
  def toChar: Char = i.toChar
  def toInt: Int = i.toInt
  def toUInt: UInt = UInt(toInt)
  def toLong: Long = i
  def toFloat: Float = {
    if (i < 0) {
      unsignedPart + ULong.unsignedRemainder
    } else unsignedPart
  }

  def toDouble: Double = {
    if (i < 0) {
      unsignedPart + ULong.unsignedRemainder
    } else unsignedPart
  }

  def unary_~ = ULong(~i)

  def <<(x: Int): ULong =
    ULong(i << x)

  def <<(x: Long): ULong =
    ULong(i << x)

  def >>(x: Int): ULong =
    ULong(i >>> x)

  def >>(x: Long): ULong =
    ULong(i >>> x)

  def ==(x: Byte): Boolean =
    i == x

  def ==(x: Short): Boolean =
    i == x

  def ==(x: Char): Boolean =
    i == x

  def ==(x: Float): Boolean =
    i == x

  def ==(x: Double): Boolean =
    i == x

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
  def <(x: Int): Boolean = java.lang.Long.compareUnsigned(i, x) < 0
  def <(x: Long): Boolean = java.lang.Long.compareUnsigned(i, x) < 0
  def <(x: UInt): Boolean = java.lang.Long.compareUnsigned(i, x.i) < 0
  def <(x: ULong): Boolean = java.lang.Long.compareUnsigned(i, x.i) < 0

  def <=(x: Byte): Boolean = i <= x
  def <=(x: Short): Boolean = i <= x
  def <=(x: Char): Boolean = i <= x
  def <=(x: Float): Boolean = i <= x
  def <=(x: Double): Boolean = i <= x
  def <=(x: Int): Boolean = java.lang.Long.compareUnsigned(i, x) <= 0
  def <=(x: Long): Boolean = java.lang.Long.compareUnsigned(i, x) <= 0
  def <=(x: UInt): Boolean = java.lang.Long.compareUnsigned(i, x.i) <= 0
  def <=(x: ULong): Boolean = java.lang.Long.compareUnsigned(i, x.i) <= 0

  def >(x: Byte): Boolean = i > x
  def >(x: Short): Boolean = i > x
  def >(x: Char): Boolean = i > x
  def >(x: Float): Boolean = i > x
  def >(x: Double): Boolean = i > x
  def >(x: Int): Boolean = java.lang.Long.compareUnsigned(i, x) > 0
  def >(x: Long): Boolean = java.lang.Long.compareUnsigned(i, x) > 0
  def >(x: UInt): Boolean = java.lang.Long.compareUnsigned(i, x.i) > 0
  def >(x: ULong): Boolean = java.lang.Long.compareUnsigned(i, x.i) > 0

  def >=(x: Byte): Boolean = i >= x
  def >=(x: Short): Boolean = i >= x
  def >=(x: Char): Boolean = i >= x
  def >=(x: Float): Boolean = i >= x
  def >=(x: Double): Boolean = i >= x
  def >=(x: Int): Boolean = java.lang.Long.compareUnsigned(i, x) >= 0
  def >=(x: Long): Boolean = java.lang.Long.compareUnsigned(i, x) >= 0
  def >=(x: UInt): Boolean = java.lang.Long.compareUnsigned(i, x.i) >= 0
  def >=(x: ULong): Boolean = java.lang.Long.compareUnsigned(i, x.i) >= 0

  def |(x: Byte): ULong = ULong(i | x)
  def |(x: Short): ULong = ULong(i | x)
  def |(x: Char): ULong = ULong(i | x)
  def |(x: Int): ULong = ULong(i | x)
  def |(x: Long): ULong = ULong(i | x)
  def |(x: UInt): ULong = ULong(i | x.i)
  def |(x: ULong): ULong = ULong(i | x.i)

  def &(x: Byte): ULong = ULong(i & x)
  def &(x: Short): ULong = ULong(i & x)
  def &(x: Char): ULong = ULong(i & x)
  def &(x: Int): ULong = ULong(i & x)
  def &(x: Long): ULong = ULong(i & x)
  def &(x: UInt): ULong = ULong(i & x.i)
  def &(x: ULong): ULong = ULong(i & x.i)

  def ^(x: Byte): ULong = ULong(i ^ x)
  def ^(x: Short): ULong = ULong(i ^ x)
  def ^(x: Char): ULong = ULong(i ^ x)
  def ^(x: Int): ULong = ULong(i ^ x)
  def ^(x: Long): ULong = ULong(i ^ x)
  def ^(x: UInt): ULong = ULong(i ^ x.i)
  def ^(x: ULong): ULong = ULong(i ^ x.i)

  def +(x: Byte): ULong = ULong(i + x)
  def +(x: Short): ULong = ULong(i + x)
  def +(x: Char): ULong = ULong(i + x)
  def +(x: Float): Float = toFloat + x
  def +(x: Double): Double = toDouble + x
  def +(x: Int): ULong = ULong(i + x)
  def +(x: Long): ULong = ULong(i + x)
  def +(x: UInt): ULong = ULong(i + x.i)
  def +(x: ULong): ULong = ULong(i + x.i)

  def -(x: Byte): ULong = ULong(i - x)
  def -(x: Short): ULong = ULong(i - x)
  def -(x: Char): ULong = ULong(i - x)
  def -(x: Float): Float = toFloat - x
  def -(x: Double): Double = toDouble - x
  def -(x: Int): ULong = ULong(i - x)
  def -(x: Long): ULong = ULong(i - x)
  def -(x: UInt): ULong = ULong(i - x.i)
  def -(x: ULong): ULong = ULong(i - x.i)

  def *(x: Byte): ULong = ULong(i * x)
  def *(x: Short): ULong = ULong(i * x)
  def *(x: Char): ULong = ULong(i * x)
  def *(x: Float): Float = toFloat * x
  def *(x: Double): Double = toDouble * x
  def *(x: Int): ULong = ULong(i * x)
  def *(x: Long): ULong = ULong(i * x)
  def *(x: UInt): ULong = ULong(i * x.i)
  def *(x: ULong): ULong = ULong(i * x.i)

  def /(x: Byte): ULong = ULong(i / x)
  def /(x: Short): ULong = ULong(i / x)
  def /(x: Char): ULong = ULong(i / x)
  def /(x: Float): Float = toFloat / x
  def /(x: Double): Double = toDouble / x
  def /(x: Int): ULong = ULong(java.lang.Long.divideUnsigned(i, x))
  def /(x: Long): ULong = ULong(java.lang.Long.divideUnsigned(i, x))
  def /(x: UInt): ULong = ULong(java.lang.Long.divideUnsigned(i, x.i))
  def /(x: ULong): ULong = ULong(java.lang.Long.divideUnsigned(i, x.i))

  def %(x: Byte): ULong = ULong(i % x)
  def %(x: Short): ULong = ULong(i % x)
  def %(x: Char): ULong = ULong(i % x)
  def %(x: Float): Float = toFloat % x
  def %(x: Double): Double = toDouble % x
  def %(x: Int): ULong = ULong(java.lang.Long.remainderUnsigned(i, x))
  def %(x: Long): ULong = ULong(java.lang.Long.remainderUnsigned(i, x))
  def %(x: UInt): ULong = ULong(java.lang.Long.remainderUnsigned(i, x.i))
  def %(x: ULong): ULong = ULong(java.lang.Long.remainderUnsigned(i, x.i))

  override def toString: String =
    java.lang.Long.toUnsignedString(i)

}

object ULong {

  final val MinValue =
    ULong(0L)

  final val MaxValue =
    ULong(-1L)

  def valueOf(x: String, radix: Int = 10): ULong =
    ULong(java.lang.Long.parseUnsignedLong(x, radix))

  private val unsignedMask =
    0x7fffffffffffffffL

  private val unsignedRemainder =
    Long.MaxValue.toFloat

  implicit def ulong2float(x: ULong): Float =
    x.toFloat

  implicit def ulong2double(x: ULong): Double =
    x.toDouble

  implicit object ULongNumeric extends Numeric[ULong] with Ordering[ULong] {
    override def plus(
      x: ULong,
      y: ULong
    ): ULong =
      x + y

    override def minus(
      x: ULong,
      y: ULong
    ): ULong =
      x - y

    override def times(
      x: ULong,
      y: ULong
    ): ULong =
      x * y

    override def negate(x: ULong): ULong =
      ULong(-x.i)

    override def fromInt(x: Int): ULong =
      ULong(x.toLong)

    override def toInt(x: ULong): Int =
      x.toInt

    override def toLong(x: ULong): Long =
      x.toLong

    override def toFloat(x: ULong): Float =
      x.toFloat

    override def toDouble(x: ULong): Double =
      x.toDouble

    override def compare(x: ULong, y: ULong): Int =
      java.lang.Long.compareUnsigned(x.i, y.i)
  }

  //avoids boxing
  class Buffer(vals: ArrayBuffer[Long]) extends scala.collection.mutable.Buffer[ULong]() {

    override def update(n: Int, newelem: ULong): Unit = vals.update(n, newelem.i)

    override def +=(elem: ULong): this.type = {
      vals += elem.i
      this
    }

    override def clear(): Unit = vals.clear()

    override def +=:(elem: ULong): this.type = {
      elem.i +=: vals
      this
    }

    override def insertAll(n: Int, elems: Traversable[ULong]): Unit = vals.insertAll(n, elems.map(_.i))

    override def remove(n: Int): ULong = {
      ULong(vals.remove(n))
    }

    override def apply(idx: Int): ULong = ULong(vals(idx))

    override def length: Int = vals.length

    override def iterator: Iterator[ULong] = {
      val underlying = vals.iterator
      new Iterator[ULong] {
        override def hasNext: Boolean = underlying.hasNext

        override def next(): ULong = ULong(underlying.next())
      }
    }

  }

  object Buffer {
    def apply(vals: ULong*): Buffer =
      new Buffer(ArrayBuffer(vals.map(_.i): _*))

    def empty: Buffer = new Buffer(ArrayBuffer.empty)
  }

}
