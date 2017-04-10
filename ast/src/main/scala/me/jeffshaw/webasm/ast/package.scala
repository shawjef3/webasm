package me.jeffshaw.webasm

import me.jeffshaw.unsigned.UInt
import scodec.bits.ByteVector

package object ast {

  type Var = UInt

  type TableSegment = Segment[Vector[Var]]

  type MemorySegment = Segment[ByteVector]

  type StackType = Vector[ValueType]

}
