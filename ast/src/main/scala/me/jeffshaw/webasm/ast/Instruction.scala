package me.jeffshaw.webasm.ast

import me.jeffshaw.unsigned.UInt
import me.jeffshaw.webasm.ast.nodes.wcodecs
import scodec._
import scodec.codecs
import scodec.bits.{BitVector, ByteVector}
import shapeless.syntax.std.product._

sealed trait Instruction

object Instruction {
  implicit val codec: Codec[Instruction] = {
    val all =
      (I32Ops.all ++
        I64Ops.all ++
        F32Ops.all ++
        F64Ops.all
        ).toSeq.map(_.codec) ++
        Seq(
          Unreachable.codec,
          Nop.codec,
          Block.codec,
          Loop.codec,
          If.codec,
          Else.codec,
          Br.codec,
          BrIf.codec,
          BrTable.codec,
          Return.codec,
          Call.codec,
          CallIndirect.codec,
          Drop.codec,
          End.codec,
          Select.codec,
          GetLocal.codec,
          SetLocal.codec,
          TeeLocal.codec,
          GetGlobal.codec,
          SetGlobal.codec,
          MemoryOperation.Load.F32None.codec,
          MemoryOperation.Load.F64None.codec,
          MemoryOperation.Load.I32Mem8SZ.codec,
          MemoryOperation.Load.I32Mem16SZ.codec,
          MemoryOperation.Load.I32None.codec,
          MemoryOperation.Load.I32Mem8ZX.codec,
          MemoryOperation.Load.I32Mem16ZX.codec,
          MemoryOperation.Load.I64Mem8SX.codec,
          MemoryOperation.Load.I64Mem8ZX.codec,
          MemoryOperation.Load.I64Mem16SX.codec,
          MemoryOperation.Load.I64Mem16ZX.codec,
          MemoryOperation.Load.I64Mem32SX.codec,
          MemoryOperation.Load.I64Mem32ZX.codec,
          MemoryOperation.Load.I64None.codec,
          MemoryOperation.Store.F32None.codec,
          MemoryOperation.Store.F64None.codec,
          MemoryOperation.Store.I32Mem8.codec,
          MemoryOperation.Store.I32Mem16.codec,
          MemoryOperation.Store.I32None.codec,
          MemoryOperation.Store.I64Mem8.codec,
          MemoryOperation.Store.I64Mem16.codec,
          MemoryOperation.Store.I64Mem32.codec,
          MemoryOperation.Store.I64None.codec,
          CurrentMemory.codec,
          GrowMemory.codec,
          Const.I32.codec,
          Const.I64.codec,
          Const.F32.codec,
          Const.F64.codec,
        )

    codecs.choice(all.asInstanceOf[Seq[Codec[Instruction]]]: _*)
  }

  trait Companion {
    type I

    val Header: Byte
    lazy val headerCodec: Codec[Unit] = codecs.constant(BitVector(Header))

    val codec: Codec[I]
  }

  trait Singleton extends Instruction with Companion {
    override type I = this.type
    override val codec: Codec[this.type] =
      headerCodec.xmap(Function.const(this), Function.const(()))
  }

  trait OnlyVar extends Companion {
    def apply(value: Var): I
    def unapply(i: I): Option[Var]

    override val codec: Codec[I] =
      headerCodec ~ wcodecs.vu32 xmap(
        x => apply(x._2),
        i => unapply(i) match {
          case Some(v) => ((), v)
          case None => ???
        }
      )
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

object Br extends Instruction.OnlyVar {
  override type I = Br
  override val Header: Byte = 0x0c
}

case class BrIf(label: Var) extends Instruction

object BrIf extends Instruction.OnlyVar {
  override type I = BrIf
  override val Header: Byte = 0x0d
}

case class BrTable(
  table: Vector[Var],
  condition: Var
) extends Instruction

object BrTable extends Instruction.Companion {
  override type I = BrTable
  override val Header: Byte = 0x0e
  override val codec: Codec[BrTable] =
    headerCodec ~ wcodecs.vec(wcodecs.vu32) ~ wcodecs.vu32 xmap(
      x => BrTable(x._1._2, x._2),
      i => unapply(i) match {
        case Some((table, condition)) => (((), table), condition)
        case None => ???
      }
    )
}

case object Return extends Instruction.Singleton {
  override val Header: Byte = 0x0f
}

case class Call(label: Var) extends Instruction

object Call extends Instruction.OnlyVar {
  override type I = Call
  override val Header: Byte = 0x10
}

case class CallIndirect(label: Var) extends Instruction

object CallIndirect extends Instruction.Companion {
  override type I = CallIndirect
  override val Header: Byte = 0x11
  override val codec: Codec[I] =
    headerCodec ~ wcodecs.vu32 ~ codecs.constant(BitVector(Array(0x00.toByte))) xmap(
      x => apply(x._1._2),
      i => unapply(i) match {
        case Some(v) => (((), v), ())
        case None => ???
      }
    )
}

case object Drop extends Instruction.Singleton {
  override val Header: Byte = 0x1a
}

case object End extends Instruction.Singleton {
  override val Header: Byte = 0x0b
}

case object Select extends Instruction.Singleton {
  override val Header: Byte = 0x1b
}

case class GetLocal(label: Var) extends Instruction

object GetLocal extends Instruction.OnlyVar {
  override type I = GetLocal
  override val Header: Byte = 0x20
}

case class SetLocal(label: Var) extends Instruction

object SetLocal extends Instruction.OnlyVar {
  override type I = SetLocal
  override val Header: Byte = 0x21
}

case class TeeLocal(label: Var) extends Instruction

object TeeLocal extends Instruction.OnlyVar {
  override type I = TeeLocal
  override val Header: Byte = 0x21
}

case class GetGlobal(label: Var) extends Instruction

object GetGlobal extends Instruction.OnlyVar {
  override type I = GetGlobal
  override val Header: Byte = 0x23
}

case class SetGlobal(label: Var) extends Instruction

object SetGlobal extends Instruction.OnlyVar {
  override type I = SetGlobal
  override val Header: Byte = 0x24
}

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

  trait M[Size] extends Instruction.Singleton {
    self =>
    val memoryType: ValueType
    val size: Option[Size]
  }

  type Load = M[Load.Size]

  object Load {
    case class Size(size: MemSize, extension: Extension)

    sealed trait Extension

    case object Extension {
      case object SX extends Extension
      case object ZX extends Extension
    }

    object I32None extends Load {
      override val Header: Byte = 0x28
      override val memoryType: ValueType = ValueType.I32
      override val size: Option[Size] = None
    }

    object I64None extends Load {
      override val Header: Byte = 0x29
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[Size] = None
    }

    object F32None extends Load {
      override val Header: Byte = 0x2a
      override val memoryType: ValueType = ValueType.F32
      override val size: Option[Size] = None
    }

    object F64None extends Load {
      override val Header: Byte = 0x2b
      override val memoryType: ValueType = ValueType.F64
      override val size: Option[Size] = None
    }

    object I32Mem8SZ extends Load {
      override val Header: Byte = 0x2c
      override val memoryType: ValueType = ValueType.I32
      override val size: Option[Size] = Some(Size(MemSize.`8`, Extension.SX))
    }

    object I32Mem8ZX extends Load {
      override val Header: Byte = 0x2d
      override val memoryType: ValueType = ValueType.I32
      override val size: Option[Size] = Some(Size(MemSize.`8`, Extension.ZX))
    }

    object I32Mem16SZ extends Load {
      override val Header: Byte = 0x2e
      override val memoryType: ValueType = ValueType.I32
      override val size: Option[Size] = Some(Size(MemSize.`16`, Extension.SX))
    }

    object I32Mem16ZX extends Load {
      override val Header: Byte = 0x2f
      override val memoryType: ValueType = ValueType.I32
      override val size: Option[Size] = Some(Size(MemSize.`16`, Extension.ZX))
    }

    object I64Mem8SX extends Load {
      override val Header: Byte = 0x30
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[Size] = Some(Size(MemSize.`8`, Extension.SX))
    }

    object I64Mem8ZX extends Load {
      override val Header: Byte = 0x31
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[Size] = Some(Size(MemSize.`8`, Extension.ZX))
    }

    object I64Mem16SX extends Load {
      override val Header: Byte = 0x32
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[Size] = Some(Size(MemSize.`16`, Extension.SX))
    }

    object I64Mem16ZX extends Load {
      override val Header: Byte = 0x33
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[Size] = Some(Size(MemSize.`16`, Extension.ZX))
    }

    object I64Mem32SX extends Load {
      override val Header: Byte = 0x34
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[Size] = Some(Size(MemSize.`32`, Extension.SX))
    }

    object I64Mem32ZX extends Load {
      override val Header: Byte = 0x35
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[Size] = Some(Size(MemSize.`32`, Extension.ZX))
    }

  }

  type Store = M[MemSize]

  object Store {

    object I32None extends Store {
      override val Header: Byte = 0x36
      override val memoryType: ValueType = ValueType.I32
      override val size: Option[MemSize] = None
    }

    object I64None extends Store {
      override val Header: Byte = 0x37
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[MemSize] = None
    }

    object F32None extends Store {
      override val Header: Byte = 0x38
      override val memoryType: ValueType = ValueType.F32
      override val size: Option[MemSize] = None
    }

    object F64None extends Store {
      override val Header: Byte = 0x39
      override val memoryType: ValueType = ValueType.F64
      override val size: Option[MemSize] = None
    }

    object I32Mem8 extends Store {
      override val Header: Byte = 0x3a
      override val memoryType: ValueType = ValueType.I32
      override val size: Option[MemSize] = Some(MemSize.`8`)
    }

    object I32Mem16 extends Store {
      override val Header: Byte = 0x3b
      override val memoryType: ValueType = ValueType.I32
      override val size: Option[MemSize] = Some(MemSize.`16`)
    }

    object I64Mem8 extends Store {
      override val Header: Byte = 0x3c
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[MemSize] = Some(MemSize.`8`)
    }

    object I64Mem16 extends Store {
      override val Header: Byte = 0x3d
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[MemSize] = Some(MemSize.`16`)
    }

    object I64Mem32 extends Store {
      override val Header: Byte = 0x3e
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[MemSize] = Some(MemSize.`32`)
    }

  }

}

case object CurrentMemory extends Instruction.Singleton {
  override val Header: Byte = 0x3f
  override lazy val headerCodec: Codec[Unit] = codecs.constant(ByteVector(Header, 0x00))
}

case object GrowMemory extends Instruction.Singleton {
  override val Header: Byte = 0x40
  override lazy val headerCodec: Codec[Unit] = codecs.constant(ByteVector(Header, 0x00))
}

trait Const[A] extends Instruction {
  val value: A
}

object Const {

  case class I32(override val value: Int) extends Const[Int]

  object I32 extends Instruction.Companion {
    override type I = I32
    override val Header: Byte = 0x41
    override val codec: Codec[I32] =
      headerCodec ~ wcodecs.vs32 xmap(
        x => I32(x._2),
        i => unapply(i) match {
          case Some(value) => ((), value)
          case None => ???
        }
      )
  }

  case class I64(override val value: Long) extends Const[Long]

  object I64 extends Instruction.Companion {
    override type I = I64
    override val Header: Byte = 0x42
    override val codec: Codec[I64] =
      headerCodec ~ wcodecs.vs64 xmap(
        x => I64(x._2),
        i => unapply(i) match {
          case Some(value) => ((), value)
          case None => ???
        }
      )
  }

  case class F32(override val value: Float) extends Const[Float]

  object F32 extends Instruction.Companion {
    override type I = F32
    override val Header: Byte = 0x43
    override val codec: Codec[F32] =
      headerCodec ~ wcodecs.vs32 xmap(
        x => F32(java.lang.Float.intBitsToFloat(x._2)),
        i => unapply(i) match {
          case Some(value) => ((), java.lang.Float.floatToIntBits(value))
          case None => ???
        }
      )
  }

  case class F64(override val value: Double) extends Const[Double]

  object F64 extends Instruction.Companion {
    override type I = F64
    override val Header: Byte = 0x44
    override val codec: Codec[F64] =
      headerCodec ~ wcodecs.vs64 xmap(
        x => F64(java.lang.Double.longBitsToDouble(x._2)),
        i => unapply(i) match {
          case Some(value) => ((), java.lang.Double.doubleToLongBits(value))
          case None => ???
        }
      )
  }

}

trait Op extends Instruction.Singleton

sealed trait Ops {
  trait Unary extends Op

  val unary: Set[Unary]

  trait Binary extends Op

  val binary: Set[Binary]

  trait Test extends Op

  val test: Set[Test]

  trait Relation extends Op

  val relation: Set[Relation]

  trait Convert extends Op

  val convert: Set[Convert]

  lazy val all: Set[Op] =
    unary ++ binary ++ test ++ relation ++ convert
}

trait IntOps extends Ops {

  def unaryOffset: Byte

  case object Clz extends Unary {
    override val Header: Byte = unaryOffset
  }
  case object Ctz extends Unary {
    override val Header: Byte = (unaryOffset + 1).toByte
  }
  case object Popcnt extends Unary {
    override val Header: Byte = (unaryOffset + 2).toByte
  }

  override val unary: Set[Unary] = Set(Clz, Ctz, Popcnt)

  def binaryOffset: Byte

  case object Add extends Binary {
    override val Header: Byte = binaryOffset
  }
  case object Sub extends Binary {
    override val Header: Byte = (binaryOffset + 1).toByte
  }
  case object Mul extends Binary {
    override val Header: Byte = (binaryOffset + 2).toByte
  }
  case object DivS extends Binary {
    override val Header: Byte = (binaryOffset + 3).toByte
  }
  case object DivU extends Binary {
    override val Header: Byte = (binaryOffset + 4).toByte
  }
  case object RemS extends Binary {
    override val Header: Byte = (binaryOffset + 5).toByte
  }
  case object RemU extends Binary {
    override val Header: Byte = (binaryOffset + 6).toByte
  }
  case object And extends Binary {
    override val Header: Byte = (binaryOffset + 7).toByte
  }
  case object Or extends Binary {
    override val Header: Byte = (binaryOffset + 8).toByte
  }
  case object Xor extends Binary {
    override val Header: Byte = (binaryOffset + 9).toByte
  }
  case object Shl extends Binary {
    override val Header: Byte = (binaryOffset + 10).toByte
  }
  case object ShrS extends Binary {
    override val Header: Byte = (binaryOffset + 11).toByte
  }
  case object ShrU extends Binary {
    override val Header: Byte = (binaryOffset + 12).toByte
  }
  case object Rotl extends Binary {
    override val Header: Byte = (binaryOffset + 13).toByte
  }
  case object Rotr extends Binary {
    override val Header: Byte = (binaryOffset + 14).toByte
  }

  override val binary: Set[Binary] =
    Set(
      Add,
      Sub,
      Mul,
      DivS,
      DivU,
      RemS,
      RemU,
      And,
      Or,
      Xor,
      Shl,
      ShrS,
      ShrU,
      Rotl,
      Rotr,
    )

  def testOffset: Byte

  case object Eqz extends Test {
    override val Header: Byte = testOffset
  }

  override val test: Set[Test] = Set(Eqz)

  def relationOffset: Byte

  case object Eq extends Relation {
    override val Header: Byte = relationOffset
  }
  case object Ne extends Relation {
    override val Header: Byte = (relationOffset + 1).toByte
  }
  case object LtS extends Relation {
    override val Header: Byte = (relationOffset + 2).toByte
  }
  case object LtU extends Relation {
    override val Header: Byte = (relationOffset + 3).toByte
  }
  case object GtS extends Relation {
    override val Header: Byte = (relationOffset + 4).toByte
  }
  case object GtU extends Relation {
    override val Header: Byte = (relationOffset + 5).toByte
  }
  case object LeS extends Relation {
    override val Header: Byte = (relationOffset + 6).toByte
  }
  case object LeU extends Relation {
    override val Header: Byte = (relationOffset + 7).toByte
  }
  case object GeS extends Relation {
    override val Header: Byte = (relationOffset + 8).toByte
  }
  case object GeU extends Relation {
    override val Header: Byte = (relationOffset + 9).toByte
  }

  override val relation: Set[Relation] =
    Set(
      Eq,
      Ne,
      LtS,
      LtU,
      GtS,
      GtU,
      LeS,
      LeU,
      GeS,
      GeU,
    )

  def convertOffset: Byte

  case object ExtendSI32 extends Convert {
    override val Header: Byte = convertOffset
  }
  case object ExtendUI32 extends Convert {
    override val Header: Byte = (convertOffset + 1).toByte
  }
  case object WrapI64 extends Convert {
    override val Header: Byte = (convertOffset + 2).toByte
  }
  case object TruncSF32 extends Convert {
    override val Header: Byte = (convertOffset + 3).toByte
  }
  case object TruncUF32 extends Convert {
    override val Header: Byte = (convertOffset + 4).toByte
  }
  case object TruncSF64 extends Convert {
    override val Header: Byte = (convertOffset + 5).toByte
  }
  case object TruncUF64 extends Convert {
    override val Header: Byte = (convertOffset + 6).toByte
  }
  case object ReinterpretFloat extends Convert {
    override val Header: Byte = (convertOffset + 7).toByte
  }

  override val convert: Set[Convert] =
    Set(
      ExtendSI32,
      ExtendUI32,
      WrapI64,
      TruncSF32,
      TruncUF32,
      TruncSF64,
      TruncUF64,
      ReinterpretFloat,
    )
}

trait FloatOps extends Ops {

  def unaryOffset: Byte

  case object Neg extends Unary {
    override val Header: Byte = unaryOffset
  }
  case object Abs extends Unary {
    override val Header: Byte = (unaryOffset + 1).toByte
  }
  case object Ceil extends Unary {
    override val Header: Byte = (unaryOffset + 2).toByte
  }
  case object Floor extends Unary {
    override val Header: Byte = (unaryOffset + 3).toByte
  }
  case object Trunc extends Unary {
    override val Header: Byte = (unaryOffset + 4).toByte
  }
  case object Nearest extends Unary {
    override val Header: Byte = (unaryOffset + 5).toByte
  }
  case object Sqrt extends Unary {
    override val Header: Byte = (unaryOffset + 6).toByte
  }

  override val unary: Set[Unary] =
    Set(
      Neg,
      Abs,
      Ceil,
      Floor,
      Trunc,
      Nearest,
      Sqrt,
    )

  def binaryOffset: Byte

  case object Add extends Binary {
    override val Header: Byte = binaryOffset
  }
  case object Sub extends Binary {
    override val Header: Byte = (binaryOffset + 1).toByte
  }
  case object Mul extends Binary {
    override val Header: Byte = (binaryOffset + 2).toByte
  }
  case object Div extends Binary {
    override val Header: Byte = (binaryOffset + 3).toByte
  }
  case object Min extends Binary {
    override val Header: Byte = (binaryOffset + 4).toByte
  }
  case object Max extends Binary {
    override val Header: Byte = (binaryOffset + 5).toByte
  }
  case object CopySign extends Binary {
    override val Header: Byte = (binaryOffset + 6).toByte
  }

  override val binary: Set[Binary] =
    Set(
      Add,
      Sub,
      Mul,
      Div,
      Min,
      Max,
      CopySign,
    )

  override val test: Set[Test] = Set()

  def relationOffset: Byte

  case object Eq extends Relation {
    override val Header: Byte = relationOffset
  }
  case object Ne extends Relation {
    override val Header: Byte = (relationOffset + 1).toByte
  }
  case object Lt extends Relation {
    override val Header: Byte = (relationOffset + 2).toByte
  }
  case object Gt extends Relation {
    override val Header: Byte = (relationOffset + 3).toByte
  }
  case object Le extends Relation {
    override val Header: Byte = (relationOffset + 4).toByte
  }
  case object Ge extends Relation {
    override val Header: Byte = (relationOffset + 5).toByte
  }


  override val relation: Set[Relation] =
    Set(
      Eq,
      Ne,
      Lt,
      Gt,
      Le,
      Gt,
    )

  def convertOffset: Byte

  case object ConvertSI32 extends Convert {
    override val Header: Byte = convertOffset
  }
  case object ConvertUI32 extends Convert {
    override val Header: Byte = (convertOffset + 1).toByte
  }
  case object ConvertSI64 extends Convert {
    override val Header: Byte = (convertOffset + 2).toByte
  }
  case object ConvertUI64 extends Convert {
    override val Header: Byte = (convertOffset + 3).toByte
  }
  case object PromoteF32 extends Convert {
    override val Header: Byte = (convertOffset + 4).toByte
  }
  case object DemoteF64 extends Convert {
    override val Header: Byte = (convertOffset + 5).toByte
  }
  case object ReinterpretInt extends Convert {
    override val Header: Byte = (convertOffset + 6).toByte
  }

  override val convert: Set[Convert] =
    Set(
      ConvertSI32,
      ConvertUI32,
      ConvertSI64,
      ConvertUI64,
      PromoteF32,
      DemoteF64,
      ReinterpretInt,
    )
}

object I32Ops extends IntOps {
  override def unaryOffset: Byte = 0x67
  override def binaryOffset: Byte = 0x6a
  override def testOffset: Byte = 0x45
  override def relationOffset: Byte = 0x46
  override def convertOffset: Byte = 0xbc.toByte
}

object I64Ops extends IntOps {
  override def unaryOffset: Byte = 0x79
  override def binaryOffset: Byte = 0x7c
  override def testOffset: Byte = 0x50
  override def relationOffset: Byte = 0x51
  override def convertOffset: Byte = 0xbd.toByte
}

object F32Ops extends FloatOps {
  override def unaryOffset: Byte = 0x8b.toByte
  override def binaryOffset: Byte = 0x92.toByte
  override def relationOffset: Byte = 0x5b
  override def convertOffset: Byte = 0xbe.toByte
}

object F64Ops extends FloatOps {
  override def unaryOffset: Byte = 0x99.toByte
  override def binaryOffset: Byte = 0xa0.toByte
  override def relationOffset: Byte = 0x61
  override def convertOffset: Byte = 0xb7.toByte
}
