package me.jeffshaw.webasm.ast

import me.jeffshaw.unsigned.{UInt, ULong}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._
import scodec.{Attempt, Codec, Decoder, DecodingContext, Encoder, SizeBound}

object wcodecs {

  val u8 =
    byte

  val u16 =
    uintL(16)

  val u32 =
    int32L.xmap[UInt](UInt(_), _.toInt)

  val u64 =
    int64L.xmap[ULong](ULong(_), _.toLong)

  private val endUDecoding =
    DecodingContext.liftAttempt(scodec.Attempt.successful(ULong.MinValue))

  val endEncoding =
    scodec.Attempt.successful(BitVector.empty)

  /**
    * LEB128 codec for unsigned integers as written in webassembly spec
    * https://github.com/WebAssembly/spec/blob/master/interpreter/spec/encode.ml.
    */
  val vu64: Codec[ULong] = {
    Codec[ULong](
      encoder =
        new Encoder[ULong] {
          override def encode(i: ULong): Attempt[BitVector] = {
            val rest = i >> 7
            val (head, tail) =
              if (rest == 0)
                (u8.encode((i & 0x7fL).toByte), endEncoding)
              else (u8.encode((i | 0x80L).toByte), vu64.encode(rest))
            for {
              h <- head
              t <- tail
            } yield h ++ t
          }

          override def sizeBound: SizeBound = SizeBound(1L, Some(10L))
        },
      decoder = {
        for {
          b <- DecodingContext(u8)
          rest <-
            if ((b & 0x80) == 0)
              endUDecoding
            else DecodingContext(vu64)
        } yield {
          val x = ULong(b & 0x7FL)
          x | (rest << 7)
        }
      }.toDecoder
    )
  }

  private val endDecoding =
    DecodingContext.liftAttempt(scodec.Attempt.successful(0L))

  /**
    * LEB128 codec for signed integers as written in webassembly spec
    * https://github.com/WebAssembly/spec/blob/master/interpreter/spec/encode.ml.
    */
  val vsN: Codec[Long] = {
    Codec[Long](
      encoder =
        new Encoder[Long] {
          override def encode(i: Long): Attempt[BitVector] = {
            val b = (i & 0x7fL).toByte
            if (-64L <= i && i < 64L)
              u8.encode(b)
            else for {
              head <- u8.encode((b | 0x80).toByte)
              tail <- vsN.encode(i >> 7)
            } yield head ++ tail
          }

          override def sizeBound: SizeBound = SizeBound(1L, Some(10L))
        },
      decoder = {
        for {
          b <- DecodingContext(u8)
          hasHighBit = (b & 0x80) == 0
          rest <-
            if (hasHighBit)
              endDecoding
            else DecodingContext(vsN)
        } yield {
          val lowBits = b & 0x7fL
          if (hasHighBit) {
            if ((b & 0x40) == 0)
              lowBits
            else lowBits | (-1L ^ 0x7fL)
          } else lowBits | (rest << 7)
        }
      }.toDecoder
    )
  }

  val vu1 = vu64.xmap[UInt](_.toUInt, _.toULong)

  val vu7 = vu64.xmap[UInt](_.toUInt, _.toULong)

  val vu32 = vu64.xmap[UInt](_.toUInt, _.toULong)

  val vs7 = vsN.xmap[Int](_.toInt, _.toLong)

  val vs32 = vsN.xmap[Int](_.toInt, _.toLong)

  val vs64 = vsN

  val bool = vu1.xmap[Boolean](l => l == UInt(1), b => if (b) UInt(1) else UInt.MinValue)

  val len32 = vu32.xmap[Int](_.toInt, UInt(_))

  val string: Codec[ByteVector] =
    Codec[ByteVector](
      encoder =
        Encoder((s: ByteVector) => {
          for {
            len <- len32.encode(s.intSize.get)
          } yield len ++ s.bits
        }),
      decoder = {
        for {
          len <- len32
          b <- bytes(len)
        } yield b
      }
    )

  def vec[A](codec: Codec[A]): Codec[Vector[A]] =
    vectorOfN(len32, codec)

  def sized[A](codec: Int => Codec[A]): Decoder[A] = {
    for {
      len <- DecodingContext(len32)
      x <- DecodingContext(codec(len).asDecoder)
    } yield x
  }.toDecoder

  /**
    * During decoding, if the header is present, skip them, and run the decoder for A.
    * During encoding, output nothing for None, otherwise output the header and the
    * encoded value for A
    */
  def ifPeek[A](header: BitVector)(implicit inner: Codec[A]): Codec[Option[A]] =
    Codec[Option[A]](
      encoder = Encoder { (x: Option[A]) =>
        x match {
          case None =>
            Attempt.successful(BitVector.empty)
          case Some(value) =>
            inner.encode(value).map(header ++ _)
        }
      },
      decoder = {
        for {
          b <- DecodingContext(recover(constant(header)))
          x <- DecodingContext(conditional(b, inner))
        } yield x
      }.toDecoder
    )

}
