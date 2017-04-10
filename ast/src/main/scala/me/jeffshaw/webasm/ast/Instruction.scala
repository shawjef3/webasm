package me.jeffshaw.webasm.ast

import me.jeffshaw.unsigned.UInt
import me.jeffshaw.webasm.ast.nodes.wcodecs
import scodec._
import scodec.codecs
import scodec.bits.BitVector
import shapeless.syntax.std.product._

sealed trait Instruction

object Instruction {
  implicit val codec: Codec[Instruction] =
    codecs.choice()

  trait Companion {
    type I

    val Header: Byte
    lazy val headerCodec = codecs.constant(BitVector(Header))

    val codec: Codec[I]
  }

  trait Singleton extends Instruction with Companion {
    override type I = this.type
    override val codec: Codec[this.type] =
      headerCodec.xmap(Function.const(this), Function.const(()))
  }
}

case object Unreachable extends Instruction.Singleton {
  override type I = this.type
  override val Header: Byte = 0x00
}

case object Nop extends Instruction.Singleton {
  override type I = this.type
  override val Header: Byte = 0x01
}

case class Block(
  stackType: StackType,
  instructions: Instructions
) extends Instruction

object Block extends Instruction.Companion {
  override type I = Block
  override val Header: Byte = 0x02
  override val codec: Codec[Block] =
    wcodecs.vec(ValueType.codec) ~ Instructions.thenEnd xmap(
      (Block.apply _).tupled,
      _.toTuple
    )
}

case class Loop(
  stackType: StackType,
  instructions: Instructions
) extends Instruction

object Loop extends Instruction.Companion {
  override type I = Loop
  override val Header: Byte = 0x03
  override val codec: Codec[Loop] =
    wcodecs.vec(ValueType.codec) ~ Instructions.thenEnd xmap(
      (Loop.apply _).tupled,
      _.toTuple
    )
}

case class If(
  stackType: StackType,
  instructions: Instructions,
  `else`: Option[Else]
) extends Instruction

object If extends Instruction.Companion {
  override type I = If
  override val Header: Byte = 0x04
  override val codec: Codec[If] = {
    val instructionsThenElse = {
      implicit val innerCodec: Codec[Option[Else]] =
        codecs.fallback(End.codec, Else.codec) xmap(
          _.toOption,
          _.toRight(End)
        )
      Instructions.codec[Option[Else]]
    }

    wcodecs.vec(ValueType.codec) ~ instructionsThenElse xmap(
      {
        case (stackType, (instructions, maybeElse)) =>
          If(stackType, instructions, maybeElse)
      },
      {
        case If(stackType, instructions, e) =>
          (stackType, (instructions, e))
      }
    )

  }

}

case class Else(instructions: Instructions) extends Instruction

object Else extends Instruction.Companion {
  override type I = Else
  override val Header: Byte = 0x05
  override val codec: Codec[Else] =
    Instructions.thenEnd xmap(
      Else(_),
      _.instructions
    )
}

case class Br(label: Var) extends Instruction

case class BrIf(label: Var) extends Instruction

case class BrTable(
  table: Vector[Var],
  condition: Var
) extends Instruction

case object Return extends Instruction

case class Call(label: Var) extends Instruction

case class CallIndirect(label: Var) extends Instruction

case object Drop extends Instruction

case object End extends Instruction.Singleton {
  override type I = this.type
  override val Header: Byte = 0x0f
}

case object Select extends Instruction

case class GetLocal(label: Var) extends Instruction

case class SetLocal(label: Var) extends Instruction

case class TeeLocal(label: Var) extends Instruction

case class GetGlobal(label: Var) extends Instruction

case class SetGlobal(label: Var) extends Instruction

sealed trait MemSize

object MemSize {
  case object `8` extends MemSize
  case object `16` extends MemSize
  case object `32` extends MemSize
}

trait MemoryOperation[Self <: MemoryOperation[Self, Size], Size] extends Instruction {
  val memoryType: ValueType
  val align: UInt
  val offset: UInt
  val size: Option[Size]
}

object MemoryOperation  {

  trait M[Size] extends Instruction {
    self =>
//    protected val HeadBytes: Vector[Byte]
    protected val memoryType: ValueType
    protected val size: Option[Size]
  }

  type Load = M[Load.Size]

  object Load {
    case class Size(size: nodes.MemSize, extension: Extension)

    sealed trait Extension

    case object Extension {
      case object SX extends Extension
      case object ZX extends Extension
    }

    object I32None extends Load {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x28)
      override protected val memoryType: ValueType = ValueType.I32
      override protected val size: Option[Size] = None
    }

    object I64None extends Load {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x29)
      override protected val memoryType: ValueType = ValueType.I64
      override protected val size: Option[Size] = None
    }

    object F32None extends Load {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x2a)
      override protected val memoryType: ValueType = ValueType.F32
      override protected val size: Option[Size] = None
    }

    object F64None extends Load {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x2b)
      override protected val memoryType: ValueType = ValueType.F64
      override protected val size: Option[Size] = None
    }

    object I32Mem8SZ extends Load {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x2c)
      override protected val memoryType: ValueType = ValueType.I32
      override protected val size: Option[Size] = Some(Size(nodes.MemSize.`8`, Extension.SX))
    }

    object I32Mem8ZX extends Load {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x2d)
      override protected val memoryType: ValueType = ValueType.I32
      override protected val size: Option[Size] = Some(Size(nodes.MemSize.`8`, Extension.ZX))
    }

    object I32Mem16SZ extends Load {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x2e)
      override protected val memoryType: ValueType = ValueType.I32
      override protected val size: Option[Size] = Some(Size(nodes.MemSize.`16`, Extension.SX))
    }

    object I32Mem16ZX extends Load {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x2f)
      override protected val memoryType: ValueType = ValueType.I32
      override protected val size: Option[Size] = Some(Size(nodes.MemSize.`16`, Extension.ZX))
    }

    object I64Mem8SX extends Load {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x30)
      override protected val memoryType: ValueType = ValueType.I64
      override protected val size: Option[Size] = Some(Size(nodes.MemSize.`8`, Extension.SX))
    }

    object I64Mem8ZX extends Load {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x31)
      override protected val memoryType: ValueType = ValueType.I64
      override protected val size: Option[Size] = Some(Size(nodes.MemSize.`8`, Extension.ZX))
    }

    object I64Mem16SX extends Load {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x32)
      override protected val memoryType: ValueType = ValueType.I64
      override protected val size: Option[Size] = Some(Size(nodes.MemSize.`16`, Extension.SX))
    }

    object I64Mem16ZX extends Load {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x33)
      override protected val memoryType: ValueType = ValueType.I64
      override protected val size: Option[Size] = Some(Size(nodes.MemSize.`16`, Extension.ZX))
    }

    object I64Mem32SX extends Load {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x34)
      override protected val memoryType: ValueType = ValueType.I64
      override protected val size: Option[Size] = Some(Size(nodes.MemSize.`32`, Extension.SX))
    }

    object I64Mem32ZX extends Load {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x35)
      override protected val memoryType: ValueType = ValueType.I64
      override protected val size: Option[Size] = Some(Size(nodes.MemSize.`32`, Extension.ZX))
    }

  }

  type Store = M[nodes.MemSize]

  object Store {

    object I32None extends Store {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x36)
      override protected val memoryType: ValueType = ValueType.I32
      override protected val size: Option[nodes.MemSize] = None
    }

    object I64None extends Store {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x37)
      override protected val memoryType: ValueType = ValueType.I64
      override protected val size: Option[nodes.MemSize] = None
    }

    object F32None extends Store {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x38)
      override protected val memoryType: ValueType = ValueType.F32
      override protected val size: Option[nodes.MemSize] = None
    }

    object F64None extends Store {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x39)
      override protected val memoryType: ValueType = ValueType.F64
      override protected val size: Option[nodes.MemSize] = None
    }

    object I32Mem8 extends Store {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x3a)
      override protected val memoryType: ValueType = ValueType.I32
      override protected val size: Option[nodes.MemSize] = Some(nodes.MemSize.`8`)
    }

    object I32Mem16 extends Store {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x3b)
      override protected val memoryType: ValueType = ValueType.I32
      override protected val size: Option[nodes.MemSize] = Some(nodes.MemSize.`16`)
    }

    object I64Mem8 extends Store {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x3c)
      override protected val memoryType: ValueType = ValueType.I64
      override protected val size: Option[nodes.MemSize] = Some(nodes.MemSize.`8`)
    }

    object I64Mem16 extends Store {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x3d)
      override protected val memoryType: ValueType = ValueType.I64
      override protected val size: Option[nodes.MemSize] = Some(nodes.MemSize.`16`)
    }

    object I64Mem32 extends Store {
      //override protected val HeadBytes: Vector[Byte] = Bytes(0x3e)
      override protected val memoryType: ValueType = ValueType.I64
      override protected val size: Option[nodes.MemSize] = Some(nodes.MemSize.`32`)
    }

  }

}

case object CurrentMemory extends Instruction

case object GrowMemory extends Instruction

trait Const[A] extends Instruction {
  val value: A
}

object Const {

  case class I32(override val value: Int) extends Const[Int] {
//    override def HeadBytes: Var = 0x41
  }

  case class I64(override val value: Long) extends Const[Long] {
//    override def HeadBytes: Var = 0x42
  }

  case class F32(override val value: Float) extends Const[Float] {
//    override def HeadBytes: Var = 0x43
  }

  case class F64(override val value: Double) extends Const[Double] {
//    override def HeadBytes: Var = 0x44
  }

}

trait Op extends Instruction

sealed trait Ops {
  trait Unary extends Op

  trait Binary extends Op

  trait Test extends Op

  trait Relation extends Op

  trait Convert extends Op
}

trait IntOps extends Ops {

  def unaryOffset: Int

  case object Clz extends Unary {
    //override protected val HeadBytes = unaryOffset
  }
  case object Ctz extends Unary {
    //override def HeadBytes: Int = unaryOffset + 1
  }
  case object Popcnt extends Unary {
    //override def HeadBytes: Int = unaryOffset + 2
  }

  def binaryOffset: Int

  case object Add extends Binary {
    //override def HeadBytes: Int = binaryOffset
  }
  case object Sub extends Binary {
    //override def HeadBytes: Int = binaryOffset + 1
  }
  case object Mul extends Binary {
    //override def HeadBytes: Int = binaryOffset + 2
  }
  case object DivS extends Binary {
    //override def HeadBytes: Int = binaryOffset + 3
  }
  case object DivU extends Binary {
    //override def HeadBytes: Int = binaryOffset + 4
  }
  case object RemS extends Binary {
    //override def HeadBytes: Int = binaryOffset + 5
  }
  case object RemU extends Binary {
    //override def HeadBytes: Int = binaryOffset + 6
  }
  case object And extends Binary {
    //override def HeadBytes: Int = binaryOffset + 7
  }
  case object Or extends Binary {
    //override def HeadBytes: Int = binaryOffset + 8
  }
  case object Xor extends Binary {
    //override def HeadBytes: Int = binaryOffset + 9
  }
  case object Shl extends Binary {
    //override def HeadBytes: Int = binaryOffset + 10
  }
  case object ShrS extends Binary {
    //override def HeadBytes: Int = binaryOffset + 11
  }
  case object ShrU extends Binary {
    //override def HeadBytes: Int = binaryOffset + 12
  }
  case object Rotl extends Binary {
    //override def HeadBytes: Int = binaryOffset + 13
  }
  case object Rotr extends Binary {
    //override def HeadBytes: Int = binaryOffset + 14
  }

  def testOffset: Int

  case object Eqz extends Test {
    //override def HeadBytes: Int = testOffset
  }

  def relationOffset: Int

  case object Eq extends Relation {
    //override def HeadBytes: Int = relationOffset
  }
  case object Ne extends Relation {
    //override def HeadBytes: Int = relationOffset + 1
  }
  case object LtS extends Relation {
    //override def HeadBytes: Int = relationOffset + 2
  }
  case object LtU extends Relation {
    //override def HeadBytes: Int = relationOffset + 3
  }
  case object GtS extends Relation {
    //override def HeadBytes: Int = relationOffset + 4
  }
  case object GtU extends Relation {
    //override def HeadBytes: Int = relationOffset + 5
  }
  case object LeS extends Relation {
    //override def HeadBytes: Int = relationOffset + 6
  }
  case object LeU extends Relation {
    //override def HeadBytes: Int = relationOffset + 7
  }
  case object GeS extends Relation {
    //override def HeadBytes: Int = relationOffset + 8
  }
  case object GeU extends Relation {
    //override def HeadBytes: Int = relationOffset + 9
  }

  def convertOffset: Int

  case object ExtendSI32 extends Convert {
    //override def HeadBytes: Int = convertOffset
  }
  case object ExtendUI32 extends Convert {
    //override def HeadBytes: Int = convertOffset + 1
  }
  case object WrapI64 extends Convert {
    //override def HeadBytes: Int = convertOffset + 2
  }
  case object TruncSF32 extends Convert {
    //override def HeadBytes: Int = convertOffset + 3
  }
  case object TruncUF32 extends Convert {
    //override def HeadBytes: Int = convertOffset + 4
  }
  case object TruncSF64 extends Convert {
    //override def HeadBytes: Int = convertOffset + 5
  }
  case object TruncUF64 extends Convert {
    //override def HeadBytes: Int = convertOffset + 6
  }
  case object ReinterpretFloat extends Convert {
    //override def HeadBytes: Int = convertOffset + 7
  }

}

trait FloatOps extends Ops {

  def unaryOffset: Int

  case object Neg extends Unary {
    //override def HeadBytes: Int = unaryOffset
  }
  case object Abs extends Unary {
    //override def HeadBytes: Int = unaryOffset + 1
  }
  case object Ceil extends Unary {
    //override def HeadBytes: Int = unaryOffset + 2
  }
  case object Floor extends Unary {
    //override def HeadBytes: Int = unaryOffset + 3
  }
  case object Trunc extends Unary {
    //override def HeadBytes: Int = unaryOffset + 4
  }
  case object Nearest extends Unary {
    //override def HeadBytes: Int = unaryOffset + 5
  }
  case object Sqrt extends Unary {
    //override def HeadBytes: Int = unaryOffset + 6
  }

  def binaryOffset: Int

  case object Add extends Binary {
    //override def HeadBytes: Int = binaryOffset
  }
  case object Sub extends Binary {
    //override def HeadBytes: Int = binaryOffset + 1
  }
  case object Mul extends Binary {
    //override def HeadBytes: Int = binaryOffset + 2
  }
  case object Div extends Binary {
    //override def HeadBytes: Int = binaryOffset + 3
  }
  case object Min extends Binary {
    //override def HeadBytes: Int = binaryOffset + 4
  }
  case object Max extends Binary {
    //override def HeadBytes: Int = binaryOffset + 5
  }
  case object CopySign extends Binary {
    //override def HeadBytes: Int = binaryOffset + 6
  }

  def testOffset: Int

  case object Eqz extends Test {
    //override def HeadBytes: Int = testOffset
  }

  def relationOffset: Int

  case object Eq extends Relation {
    //override def HeadBytes: Int = relationOffset
  }
  case object Ne extends Relation {
    //override def HeadBytes: Int = relationOffset + 1
  }
  case object Lt extends Relation {
    //override def HeadBytes: Int = relationOffset + 2
  }
  case object Gt extends Relation {
    //override def HeadBytes: Int = relationOffset + 3
  }
  case object Le extends Relation {
    //override def HeadBytes: Int = relationOffset + 4
  }
  case object Ge extends Relation {
    //override def HeadBytes: Int = relationOffset + 5
  }

  def convertOffset: Int

  case object ConvertSI32 extends Convert {
    //override def HeadBytes: Int = convertOffset
  }
  case object ConvertUI32 extends Convert {
    //override def HeadBytes: Int = convertOffset + 1
  }
  case object ConvertSI64 extends Convert {
    //override def HeadBytes: Int = convertOffset + 2
  }
  case object ConvertUI64 extends Convert {
    //override def HeadBytes: Int = convertOffset + 3
  }
  case object PromoteF32 extends Convert {
    //override def HeadBytes: Int = convertOffset + 4
  }
  case object DemoteF64 extends Convert {
    //override def HeadBytes: Int = convertOffset + 5
  }
  case object ReinterpretInt extends Convert {
    //override def HeadBytes: Int = convertOffset + 6
  }

}

object I32Ops extends IntOps {
  override def unaryOffset: Int = 0x67
  override def binaryOffset: Int = 0x6a
  override def testOffset: Int = 0x45
  override def relationOffset: Int = 0x46
  override def convertOffset: Int = 0xbc
}

object I64Ops extends IntOps {
  override def unaryOffset: Int = 0x79
  override def binaryOffset: Int = 0x7c
  override def testOffset: Int = 0x50
  override def relationOffset: Int = 0x51
  override def convertOffset: Int = 0xbd
}

object F32Ops extends FloatOps {
  override def unaryOffset: Int = 0x8b
  override def binaryOffset: Int = 0x92
  override def testOffset: Int = ???
  override def relationOffset: Int = 0x5b
  override def convertOffset: Int = 0xbe
}

object F64Ops extends FloatOps {
  override def unaryOffset: Int = 0x99
  override def binaryOffset: Int = 0xa0
  override def testOffset: Int = ???
  override def relationOffset: Int = 0x61
  override def convertOffset: Int = 0xb7
}
