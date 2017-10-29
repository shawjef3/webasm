package me.jeffshaw.webasm.ast

import scodec.{Codec, codecs}

sealed trait ElemType

object ElemType {

  case object AnyFuncType extends ElemType

  implicit val codec: Codec[ElemType] =
    codecs.constant(wcodecs.u8.encode(0x20).require).xmap(
      Function.const(AnyFuncType),
      Function.const(())
    )
}
