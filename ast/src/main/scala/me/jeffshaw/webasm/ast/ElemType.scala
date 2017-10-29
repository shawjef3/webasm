package me.jeffshaw.webasm.ast

import scodec.Codec

sealed trait ElemType

object ElemType {

  case object AnyFuncType extends ElemType

  implicit val codec: Codec[ElemType] =
    wcodecs.u8Const(0x20).xmap(
      Function.const(AnyFuncType),
      Function.const(())
    )
}
