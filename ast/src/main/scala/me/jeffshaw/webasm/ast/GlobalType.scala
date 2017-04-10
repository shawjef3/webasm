package me.jeffshaw.webasm.ast

import scodec.Codec

case class GlobalType(
  valueType: ValueType,
  mutability: Mutability
)

object GlobalType {
  implicit val codec: Codec[GlobalType] =
    ValueType.codec ~ Codec[Mutability] xmap[GlobalType](
      (GlobalType.apply _).tupled,
      (g: GlobalType) => (g.valueType, g.mutability)
    )
}
