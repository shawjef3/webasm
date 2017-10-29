package me.jeffshaw.webasm.ast

import scodec.Codec

case class FuncType(
  argumentTypes: StackType,
  returns: StackType
)

object FuncType {
  implicit val codec: Codec[FuncType] =
    wcodecs.u8Const(0x40) ~
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
