package me.jeffshaw.webasm.ast

import scodec.Codec

case class Memory(memoryType: MemoryType)

object Memory {
  implicit val codec: Codec[Memory] =
    Codec[MemoryType].xmap(
      Memory(_),
      _.memoryType
    )
}
