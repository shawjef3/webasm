package me.jeffshaw.webasm.ast

import me.jeffshaw.unsigned.UInt
import scodec.Codec

case class MemoryType(memoryType: Limits[UInt])

object MemoryType {
  implicit val codec: Codec[MemoryType] =
    Codec[Limits[UInt]].xmap(
      MemoryType(_),
      _.memoryType
    )
}
