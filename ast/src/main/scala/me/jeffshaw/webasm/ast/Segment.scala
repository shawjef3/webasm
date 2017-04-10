package me.jeffshaw.webasm.ast

import me.jeffshaw.webasm.ast.nodes.wcodecs
import scodec.Codec

case class Segment[A](
  index: Var,
  offset: Instructions, //const
  init: A
)

object Segment {
  implicit def codec[A](implicit initCodec: Codec[A]): Codec[Segment[A]] =
    wcodecs.vu32 ~ Instructions.thenEnd ~ initCodec xmap(
      {
        case ((index, offset), init) =>
          Segment(index, offset, init)
      },
      (s: Segment[A]) =>
        ((s.index, s.offset), s.init)
    )
}
