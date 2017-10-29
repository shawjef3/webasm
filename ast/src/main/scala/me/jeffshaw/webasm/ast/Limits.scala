package me.jeffshaw.webasm.ast

import me.jeffshaw.unsigned.UInt
import scodec.bits.BitVector
import scodec._

case class Limits[A](min: A, max: Option[A])

object Limits {
  def codecAux[A](inner: Codec[A]): Codec[Limits[A]] =
    Codec(
      encoder =
        Encoder { (l: Limits[A]) =>
          for {
            b <- wcodecs.bool.encode(l.max.nonEmpty)
            min <- inner.encode(l.min)
            max <- l.max.map(inner.encode).getOrElse(Attempt.successful(BitVector.empty))
          } yield b ++ min ++ max
        },
      decoder = {
                  for {
                    b <- DecodingContext(wcodecs.bool)
                    min <- DecodingContext(inner)
                    max <- DecodingContext(codecs.optional(codecs.provide(b), inner))
                  } yield Limits(min, max)
                }.toDecoder
    )

  implicit val intCodec: Codec[Limits[UInt]] =
    codecAux[UInt](wcodecs.vu32)
}
