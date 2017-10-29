package me.jeffshaw.webasm.ast

import scodec.{Codec, codecs}

case class FuncType(
  argumentTypes: StackType,
  returns: StackType
)

object FuncType {
  implicit val codec: Codec[FuncType] =
    codecs.constant(wcodecs.u8.encode(0x40).require) ~
      wcodecs.vec(ValueType.codec) ~
      wcodecs.vec(ValueType.codec) xmap(
      {
        case (((), argumentTypes), returns) =>
          FuncType(argumentTypes, returns)
      },
      {
        case FuncType(argumentTypes, returns) =>
          (((), argumentTypes), returns)
      }
    )
}
