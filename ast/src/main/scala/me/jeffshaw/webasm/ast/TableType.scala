package me.jeffshaw.webasm.ast

import me.jeffshaw.unsigned.UInt
import scodec.Codec

case class TableType(
  limits: Limits[UInt],
  elemType: ElemType
)

object TableType {
  implicit val codec: Codec[TableType] =
    Codec[ElemType] ~ Codec[Limits[UInt]] xmap(
      (t: (ElemType, Limits[UInt])) => TableType(t._2, t._1),
      t => (t.elemType, t.limits)
    )
}
