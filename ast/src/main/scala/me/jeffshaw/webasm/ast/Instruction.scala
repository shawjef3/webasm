package me.jeffshaw.webasm.ast

import me.jeffshaw.unsigned.UInt
import scala.reflect.ClassTag
import scodec._
import scodec.codecs
import scodec.bits.{BitVector, ByteVector}
import shapeless.syntax.std.product._

sealed trait Instruction

object Instruction {
  val all: Seq[Instruction.Companion] =
    (I32Ops.all ++
      I64Ops.all ++
      F32Ops.all ++
      F64Ops.all
      ).toSeq ++
      Seq(
        Unreachable,
        Nop,
        Block,
        Loop,
        If,
        Else,
        Br,
        BrIf,
        BrTable,
        Return,
        Call,
        CallIndirect,
        Drop,
        End,
        Select,
        GetLocal,
        SetLocal,
        TeeLocal,
        GetGlobal,
        SetGlobal,
        MemoryOperation.Load.F32None,
        MemoryOperation.Load.F64None,
        MemoryOperation.Load.I32Mem8SZ,
        MemoryOperation.Load.I32Mem16SZ,
        MemoryOperation.Load.I32None,
        MemoryOperation.Load.I32Mem8ZX,
        MemoryOperation.Load.I32Mem16ZX,
        MemoryOperation.Load.I64Mem8SX,
        MemoryOperation.Load.I64Mem8ZX,
        MemoryOperation.Load.I64Mem16SX,
        MemoryOperation.Load.I64Mem16ZX,
        MemoryOperation.Load.I64Mem32SX,
        MemoryOperation.Load.I64Mem32ZX,
        MemoryOperation.Load.I64None,
        MemoryOperation.Store.F32None,
        MemoryOperation.Store.F64None,
        MemoryOperation.Store.I32Mem8,
        MemoryOperation.Store.I32Mem16,
        MemoryOperation.Store.I32None,
        MemoryOperation.Store.I64Mem8,
        MemoryOperation.Store.I64Mem16,
        MemoryOperation.Store.I64Mem32,
        MemoryOperation.Store.I64None,
        CurrentMemory,
        GrowMemory,
        Const.I32,
        Const.I64,
        Const.F32,
        Const.F64
      )

  implicit val codec: Codec[Instruction] =
    codecs.choice(all.map(_.codec): _*).asInstanceOf[Codec[Instruction]]

  implicit val sCodec: Sexpr.Codec[Instruction] = {
    val sExprCodecs: Seq[Sexpr.Codec.Partial[Instruction]] =
      all.collect {
        case s: IsSexpr =>
          s.sCodec.asInstanceOf[Sexpr.Codec.Partial[Instruction]]
      }

    new Sexpr.Codec.Partial[Instruction] {
      override val encoder: PartialFunction[Instruction, Sexpr] =
        sExprCodecs.foldLeft(PartialFunction.empty[Instruction, Sexpr]) {
          case (accum, f) =>
            accum.orElse(f.encoder)
        }

      override val decoder: PartialFunction[Sexpr, Instruction] =
        sExprCodecs.foldLeft(PartialFunction.empty[Sexpr, Instruction]) {
          case (accum, f) =>
            accum.orElse(f.decoder)
        }
    }
  }

  trait Companion {
    type I <: Instruction

    val Header: Byte
    val headerCodec: Codec[Unit] = codecs.constant(BitVector(Header))

    implicit val codec: Codec[I]
  }

  trait IsSexpr {
    self: Companion =>
    implicit val sCodec: Sexpr.Codec.Partial[I]
  }

  /**
    * The atom is the same each time.
    */
  trait IsSexprFull extends IsSexpr {
    self: Companion =>
    val SName: String

    val AsAtom: Sexpr = Sexpr.Atom(SName)
  }

  trait Singleton extends Instruction with Companion {
    override type I = this.type
    override implicit val codec: Codec[this.type] =
      headerCodec.xmap(Function.const(this), Function.const(()))
  }

  object Singleton {
    trait IsSexpr extends Singleton with Instruction.IsSexprFull {
      override implicit val sCodec: Sexpr.Codec.Partial[I] =
        new Sexpr.Codec.Partial[I] {
          override val encoder: PartialFunction[IsSexpr.this.type, Sexpr] = {
            case _: I => AsAtom
          }
          override val decoder: PartialFunction[Sexpr, IsSexpr.this.type] = {
            case Sexpr.Singleton(AsAtom) =>
              IsSexpr.this
          }
        }
    }
  }

  trait OnlyVar extends Companion with IsSexprFull {
    def apply(value: Var): I
    def unapply(i: I): Option[Var]

    object This {
      def unapply(i: I): Option[Var] =
        OnlyVar.this.unapply(i)
    }

    override implicit val codec: Codec[I] =
      headerCodec ~ wcodecs.vu32 xmap(
        x => apply(x._2),
        i => unapply(i) match {
          case Some(v) => ((), v)
          case None => ???
        }
      )
    override val sCodec: Sexpr.Codec.Partial[I] =
      new Sexpr.Codec.Partial[I] {
        override val encoder: PartialFunction[I, Sexpr] = {
          case This(v) =>
            AsAtom ++ Sexpr.Utils.int32.encode(v)
        }
        override val decoder: PartialFunction[Sexpr, I] = {
          case Sexpr.Cons(Sexpr.Singleton(AsAtom), value: Sexpr.Atom) =>
            apply(Sexpr.Utils.int32.decode(value))
        }
      }
  }
}

case object Unreachable extends {
  override val Header: Byte = 0x00
  override val SName: String = "unreachable"
} with Instruction.Singleton with Instruction.Singleton.IsSexpr {
  override type I = this.type
}

case object Nop extends {
  override val Header: Byte = 0x01
  override val SName: String = "nop"
} with Instruction.Singleton with Instruction.Singleton.IsSexpr {
  override type I = this.type
}

case class Block(
  stackType: StackType,
  instructions: Instructions
) extends Instruction

object Block extends {
  override val SName: String = "block"
  override val Header: Byte = 0x02
} with Instruction.Companion with Instruction.IsSexprFull {
  override type I = Block
  override implicit val codec: Codec[Block] =
    StackType.codec ~ Instructions.thenEnd xmap(
      (Block.apply _).tupled,
      _.toTuple
    )

  override implicit val sCodec: Sexpr.Codec.Partial[Block] =
    new Sexpr.Codec.Partial[Block] {
      override val encoder: PartialFunction[Block, Sexpr] = {
        case Block(stackType, instructions) =>
          Sexpr.Atom(SName) ++
            Sexpr.Codec[StackType].encode(stackType) ++
            Sexpr.Codec[Instructions].encode(instructions)
      }

      override val decoder: PartialFunction[Sexpr, Block] = {
        case Sexpr.Node(Sexpr.Singleton(Sexpr.Atom(SName)) +: Sexpr.Singleton(encodedStackType) +: encodedInstructions) =>
          val stackType = Sexpr.Codec[StackType].decode(encodedStackType)
          val instructions = Sexpr.Codec[Instructions].decode(Sexpr.Node(encodedInstructions))
          Block(stackType, instructions)
      }
    }
}

case class Loop(
  stackType: StackType,
  instructions: Instructions
) extends Instruction

object Loop extends {
  override val SName: String = "loop"
  override val Header: Byte = 0x03
} with Instruction.Companion with Instruction.IsSexprFull {
  override type I = Loop
  override implicit val codec: Codec[Loop] =
    StackType.codec ~ Instructions.thenEnd xmap(
      (Loop.apply _).tupled,
      _.toTuple
    )

  override implicit val sCodec: Sexpr.Codec.Partial[Loop] =
    new Sexpr.Codec.Partial[Loop] {
      override val encoder: PartialFunction[Loop, Sexpr] = {
        case Loop(stackType, instructions) =>
          Sexpr.Atom(SName) ++
            Sexpr.Codec[StackType].encode(stackType) ++
            Sexpr.Codec[Instructions].encode(instructions)
      }

      override val decoder: PartialFunction[Sexpr, Loop] = {
        case Sexpr.Node(Sexpr.Singleton(Sexpr.Atom(SName)) +: Sexpr.Singleton(encodedStackType) +: encodedInstructions) =>
          val stackType = Sexpr.Codec[StackType].decode(encodedStackType)
          val instructions = Sexpr.Codec[Instructions].decode(Sexpr.Node(encodedInstructions))
          Loop(stackType, instructions)
      }
    }
}

case class If(
  stackType: StackType,
  instructions: Instructions,
  `else`: Option[Else]
) extends Instruction

object If extends {
  override val SName: String = "if"
  override val Header: Byte = 0x04
} with Instruction.Companion with Instruction.IsSexprFull {
  override type I = If
  override implicit val codec: Codec[If] = {
    val elseOrEnd: Codec[Option[Else]] =
      codecs.fallback(End.codec, Else.codec) xmap(
        _.right.toOption,
        Function.const(Left(End))
      )

    Codec[StackType] ~ Instructions.thenNothing ~ elseOrEnd xmap(
      {
        case ((stackType, instructions), maybeElse) =>
          If(stackType, instructions, maybeElse)
      },
      {
        case If(stackType, instructions, e) =>
          ((stackType, instructions), e)
      }
    )
  }

  override implicit val sCodec: Sexpr.Codec.Partial[If] =
    new Sexpr.Codec.Partial[If] {
      override val encoder: PartialFunction[If, Sexpr] = {
        case If(stackType, instructions, maybeElse) =>
          AsAtom ++
            Sexpr.Codec[StackType].encode(stackType) ++
            Sexpr.Node(Sexpr.Node(Sexpr.Atom("then") ++ Sexpr.Codec[Instructions].encode(instructions))) ++
          Sexpr.Node(Sexpr.Codec[Else].encode(maybeElse.getOrElse(Else.empty)))
      }
      override val decoder: PartialFunction[Sexpr, If] = {
        case Sexpr.Node(Vector(Sexpr.Singleton(AsAtom), stackTypeSexpr, Sexpr.Node(Sexpr.Atom("then") +: instructionsSexpr), elseSexpr@Sexpr.Node(Else.AsAtom +: _))) =>
          val stackType = Sexpr.Codec[StackType].decode(stackTypeSexpr)
          val instructions = Sexpr.Codec[Instructions].decode(Sexpr.Node(instructionsSexpr))
          val decodedElse = Sexpr.Codec[Else].decode(elseSexpr)
          val `else` =
            if (decodedElse.isEmpty)
              None
            else Some(decodedElse)
          If(stackType, instructions, `else`)
      }
    }
}

case class Else(instructions: Instructions) extends Instruction {
  def isEmpty: Boolean = instructions.instructions.isEmpty
}

object Else extends {
  override val Header: Byte = 0x05
  override val SName: String = "else"
} with Instruction.Companion with Instruction.IsSexprFull {
  override type I = Else
  override implicit val codec: Codec[Else] =
    Instructions.thenEnd xmap(
      Else(_),
      _.instructions
    )

  override implicit val sCodec: Sexpr.Codec.Partial[Else] =
    new Sexpr.Codec.Partial[Else] {
      override val encoder: PartialFunction[Else, Sexpr] = {
        case e: Else =>
          AsAtom ++ Sexpr.Codec[Instructions].encode(e.instructions)
      }
      override val decoder: PartialFunction[Sexpr, Else] = {
        case Sexpr.Cons(AsAtom, instructionsSexpr) =>
          val instructions = Sexpr.Codec[Instructions].decode(instructionsSexpr)
          Else(instructions)
      }
    }

  val empty = Else(Instructions.empty)

}

case class Br(label: Var) extends Instruction

object Br extends {
  override val SName: String = "br"
  override val Header: Byte = 0x0c
} with Instruction.OnlyVar with Instruction.IsSexprFull {
  override type I = Br
}

case class BrIf(label: Var) extends Instruction

object BrIf extends {
  override val SName: String = "br_if"
  override val Header: Byte = 0x0d
} with Instruction.OnlyVar with Instruction.IsSexprFull {
  override type I = BrIf
}

case class BrTable(
  table: Vector[Var],
  condition: Var
) extends Instruction

object BrTable extends {
  override val SName: String = "br_table"
  override val Header: Byte = 0x0e
} with Instruction.Companion with Instruction.IsSexprFull  {
  override type I = BrTable
  override implicit val codec: Codec[BrTable] =
    headerCodec ~ wcodecs.vec(wcodecs.vu32) ~ wcodecs.vu32 xmap(
      x => BrTable(x._1._2, x._2),
      i => unapply(i) match {
        case Some((table, condition)) => (((), table), condition)
        case None => ???
      }
    )
  override implicit val sCodec: Sexpr.Codec.Partial[BrTable] =
    new Sexpr.Codec.Partial[BrTable] {
      override val encoder: PartialFunction[BrTable, Sexpr] = {
        case BrTable(table, condition) =>
          Sexpr.Node(AsAtom +: (table :+ condition).map(Sexpr.Utils.int32.encode))
      }
      override val decoder: PartialFunction[Sexpr, BrTable] = {
        case Sexpr.Cons(AsAtom, Sexpr.Node(tableSexpr :+ conditionSexpr)) =>
          val table = tableSexpr.map(Sexpr.Utils.int32.decode)
          val condition = Sexpr.Utils.int32.decode(conditionSexpr)
          BrTable(table, condition)
      }
    }
}

case object Return extends {
  override val SName: String = "return"
  override val Header: Byte = 0x0f
} with Instruction.Singleton.IsSexpr

case class Call(label: Var) extends Instruction

object Call extends {
  override val SName: String = "call"
  override val Header: Byte = 0x10
} with Instruction.OnlyVar {
  override type I = Call
}

case class CallIndirect(label: Var) extends Instruction

object CallIndirect extends {
  override val SName: String = "call_indirect"
  override val Header: Byte = 0x11
} with Instruction.OnlyVar {
  override type I = CallIndirect
  override implicit val codec: Codec[I] =
    headerCodec ~ wcodecs.vu32 ~ wcodecs.u8Const(0x00) xmap(
      x => apply(x._1._2),
      i => unapply(i) match {
        case Some(v) => (((), v), ())
        case None => ???
      }
    )
}

case object Drop extends {
  override val SName: String = "drop"
  override val Header: Byte = 0x1a
} with Instruction.Singleton.IsSexpr

case object End extends {
  override val Header: Byte = 0x0b
  override val SName: String = ???
} with Instruction.Singleton.IsSexpr

case object Select extends {
  override val Header: Byte = 0x1b
  override val SName: String = "select"
} with Instruction.Singleton.IsSexpr

case class GetLocal(label: Var) extends Instruction

object GetLocal extends {
  override val Header: Byte = 0x20
  override val SName: String = "get_local"
} with Instruction.OnlyVar {
  override type I = GetLocal
}

case class SetLocal(label: Var) extends Instruction

object SetLocal extends {
  override val Header: Byte = 0x21
  override val SName: String = "set_local"
} with Instruction.OnlyVar {
  override type I = SetLocal
}

case class TeeLocal(label: Var) extends Instruction

object TeeLocal extends {
  override val Header: Byte = 0x21
  override val SName: String = "TeeLocal"
} with Instruction.OnlyVar {
  override type I = TeeLocal
}

case class GetGlobal(label: Var) extends Instruction

object GetGlobal extends {
  override val Header: Byte = 0x23
  override val SName: String = "get_global"
} with Instruction.OnlyVar {
  override type I = GetGlobal
}

case class SetGlobal(label: Var) extends Instruction

object SetGlobal extends {
  override val Header: Byte = 0x24
  override val SName: String = "set_global"
} with Instruction.OnlyVar {
  override type I = SetGlobal
}

sealed trait MemSize extends Sexpr.Part

object MemSize {
  case object `8` extends MemSize {
    override val sExprPart: String = "8"
  }
  case object `16` extends MemSize {
    override val sExprPart: String = "16"
  }
  case object `32` extends MemSize {
    override val sExprPart: String = "32"
  }
}

trait MemoryOperation[Size] extends Instruction {
  val memoryType: ValueType
  val align: Int
  val offset: UInt
  val size: Option[Size]
}

object MemoryOperation {

  object Offset {
    def apply(offset: UInt): Sexpr = {
      if (offset == UInt.MinValue)
        Sexpr.Node.empty
      else Sexpr.Atom("offset=0x" + offset.toString(16))
    }

    val matcher = "offset=(\\d+)".r

    def unapply(offsetString: String): Option[UInt] = {
      val radix =
        if (offsetString.startsWith("0x"))
          16
        else 10
      Some(UInt.valueOf(offsetString, radix))
    }
  }

  object Align {
    def apply(align: Int, valueType: ValueType): Sexpr = {
      Sexpr.Atom("align=" + PowerOf2(align, valueType))
    }

    def unapply(s: String): Option[Int] = {
      PowerOf2.unapply(s)
    }

    object PowerOf2 {
      def apply(align: Int, valueType: ValueType): String = {
        val shifted = 1 << align
        "align=" + shifted
      }

      def unapply(alignString: String): Option[Int] = {
        val asInt = alignString.toInt
        if (isPowerOfTwo(asInt)) {
          Some(log2(asInt))
        } else None
      }

      def isPowerOfTwo(n: Int): Boolean = {
        n > 0 && (n & (n - 1)) == 0
      }

      //https://graphics.stanford.edu/~seander/bithacks.html
      private val logTable256 = {
        val a = Array.fill(256)(0)
        for (i <- 2 until 256) {
          a(i) = 1 + a(i / 2)
        }
        a(0) = -1
        a
      }

      def log2(v: Int): Int = {
        var t = 0
        var tt = 0
        if ({tt = v >> 16; tt != 0}) {
          t = tt >> 8
          if (t == 0) {
            16 + logTable256(tt)
          } else 24 + logTable256(t)
        } else {
          t = v >> 8
          if (t == 0) {
            logTable256(v)
          } else 8 + logTable256(t)
        }
      }
    }
  }

  trait Companion[Self <: MemoryOperation[Size], Size] extends Instruction.Companion with Instruction.IsSexprFull {
    self =>
    def apply(align: Int, offset: UInt): Self
    def unapply(value: Self): Option[(Int, UInt)]

    override type I = Self

    val memoryType: ValueType
    val size: Option[Size]

    override implicit val codec: Codec[Self] = ???

    implicit val selfTag: ClassTag[Self]

    override val sCodec: Sexpr.Codec.Partial[Self] =
      new Sexpr.Codec.Partial[Self] {
        override val encoder: PartialFunction[Self, Sexpr] = {
          case value: Self =>
            val offset = Offset(value.offset)
            val align = Align(value.align, value.memoryType)
            Sexpr.Node(AsAtom ++ offset ++ align)
        }

        override val decoder: PartialFunction[Sexpr, Self] = {
          case Sexpr.Node(AsAtom +: tail) =>
            // From offset_opt and align_opt rules
            val map = Companion.eqMap(tail)
            (map.getOrElse("align", memoryType.size.toString), map.getOrElse("offset", "0")) match {
              case (Align(align), Offset(offset)) =>
                apply(align, offset)
              case _ =>
                throw new IllegalArgumentException()
            }
        }
      }
  }

  object Companion {
    def eqMap(ss: Vector[Sexpr]): Map[String, String] = {
      ss.collect {
        case Sexpr.Singleton(Sexpr.Atom(value)) if value.contains('=') =>
          val split = value.split("=", 2)
          split(0).trim -> split(1).trim
      }
    }.toMap
  }

  abstract class Load[Self <: Load[Self]](companion: Load.Companion[Self]) extends MemoryOperation[Load.Size] {
    override val memoryType: ValueType = companion.memoryType
    override val size: Option[Load.Size] = companion.size
  }

  object Load {
    abstract class Companion[Self <: Load[Self]] extends MemoryOperation.Companion[Self, Size] {
      override val SName: String = {
        val name =
          "load" + {
            for (Load.Size(size, extension) <- size) yield {
              size.sExprPart + extension.sExprPart
            }
          }.getOrElse("")

        memoryType.sExprPart + "." + name
      }
    }

    case class Size(size: MemSize, extension: Extension)

    sealed trait Extension extends Sexpr.Part

    case object Extension {
      case object SX extends Extension {
        override val sExprPart: String = "_s"
      }
      case object ZX extends Extension {
        override val sExprPart: String = "_u"
      }
    }

    case class I32None(
      override val align: Int,
      override val offset: UInt
    ) extends Load[I32None](I32None)

    object I32None extends {
      override val Header: Byte = 0x28
      override val memoryType: ValueType = ValueType.I32
      override val size: Option[Size] = None
      implicit val selfTag: ClassTag[I32None] = ClassTag(classOf[I32None])
    } with Companion[I32None]

    case class I64None(
      override val align: Int,
      override val offset: UInt
    ) extends Load[I64None](I64None)

    object I64None extends {
      override val Header: Byte = 0x29
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[Size] = None
      implicit val selfTag: ClassTag[I64None] = ClassTag(classOf[I64None])
    } with Companion[I64None]

    case class F32None(
      override val align: Int,
      override val offset: UInt
    ) extends Load[F32None](F32None)

    object F32None extends {
      override val Header: Byte = 0x2a
      override val memoryType: ValueType = ValueType.F32
      override val size: Option[Size] = None
      override implicit val selfTag: ClassTag[F32None] = ClassTag(classOf[F32None])
    } with Companion[F32None]

    case class F64None(
      override val align: Int,
      override val offset: UInt
    ) extends Load[F64None](F64None)

    object F64None extends {
      override val Header: Byte = 0x2b
      override val memoryType: ValueType = ValueType.F64
      override val size: Option[Size] = None
      override implicit val selfTag: ClassTag[F64None] = ClassTag(classOf[F64None])
    } with Companion[F64None]

    case class I32Mem8SZ(
      override val align: Int,
      override val offset: UInt
    ) extends Load[I32Mem8SZ](I32Mem8SZ)

    object I32Mem8SZ extends {
      override val Header: Byte = 0x2c
      override val memoryType: ValueType = ValueType.I32
      override val size: Option[Size] = Some(Size(MemSize.`8`, Extension.SX))
      override implicit val selfTag: ClassTag[I32Mem8SZ] = ClassTag(classOf[I32Mem8SZ])
    } with Companion[I32Mem8SZ]

    case class I32Mem8ZX(
      override val align: Int,
      override val offset: UInt
    ) extends Load[I32Mem8ZX](I32Mem8ZX)

    object I32Mem8ZX extends {
      override val Header: Byte = 0x2d
      override val memoryType: ValueType = ValueType.I32
      override val size: Option[Size] = Some(Size(MemSize.`8`, Extension.ZX))
      override implicit val selfTag: ClassTag[I32Mem8ZX] = ClassTag(classOf[I32Mem8ZX])
    } with Companion[I32Mem8ZX]

    case class I32Mem16SZ(
      override val align: Int,
      override val offset: UInt
    ) extends Load[I32Mem16SZ](I32Mem16SZ)

    object I32Mem16SZ extends {
      override val Header: Byte = 0x2e
      override val memoryType: ValueType = ValueType.I32
      override val size: Option[Size] = Some(Size(MemSize.`16`, Extension.SX))
      override implicit val selfTag: ClassTag[I32Mem16SZ] = ClassTag(classOf[I32Mem16SZ])
    } with Companion[I32Mem16SZ]

    case class I32Mem16ZX(
      override val align: Int,
      override val offset: UInt
    ) extends Load[I32Mem16ZX](I32Mem16ZX)

    object I32Mem16ZX extends {
      override val Header: Byte = 0x2f
      override val memoryType: ValueType = ValueType.I32
      override val size: Option[Size] = Some(Size(MemSize.`16`, Extension.ZX))
      override implicit val selfTag: ClassTag[I32Mem16ZX] = ClassTag(classOf[I32Mem16ZX])
    } with Companion[I32Mem16ZX]

    case class I64Mem8SX(
      override val align: Int,
      override val offset: UInt
    ) extends Load[I64Mem8SX](I64Mem8SX)

    object I64Mem8SX extends {
      override val Header: Byte = 0x30
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[Size] = Some(Size(MemSize.`8`, Extension.SX))
      override implicit val selfTag: ClassTag[I64Mem8SX] = ClassTag(classOf[I64Mem8SX])
    } with Companion[I64Mem8SX]

    case class I64Mem8ZX(
      override val align: Int,
      override val offset: UInt
    ) extends Load[I64Mem8ZX](I64Mem8ZX)

    object I64Mem8ZX extends {
      override val Header: Byte = 0x31
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[Size] = Some(Size(MemSize.`8`, Extension.ZX))
      override implicit val selfTag: ClassTag[I64Mem8ZX] = ClassTag(classOf[I64Mem8ZX])
    } with Companion[I64Mem8ZX]

    case class I64Mem16SX(
      override val align: Int,
      override val offset: UInt
    ) extends Load[I64Mem16SX](I64Mem16SX)

    object I64Mem16SX extends {
      override val Header: Byte = 0x32
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[Size] = Some(Size(MemSize.`16`, Extension.SX))
      override implicit val selfTag: ClassTag[I64Mem16SX] = ClassTag(classOf[I64Mem16SX])
    } with Companion[I64Mem16SX]

    case class I64Mem16ZX(
      override val align: Int,
      override val offset: UInt
    ) extends Load[I64Mem16ZX](I64Mem16ZX)

    object I64Mem16ZX extends {
      override val Header: Byte = 0x33
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[Size] = Some(Size(MemSize.`16`, Extension.ZX))
      override implicit val selfTag: ClassTag[I64Mem16ZX] = ClassTag(classOf[I64Mem16ZX])
    } with Companion[I64Mem16ZX]

    case class I64Mem32SX(
      override val align: Int,
      override val offset: UInt
    ) extends Load[I64Mem32SX](I64Mem32SX)

    object I64Mem32SX extends {
      override val Header: Byte = 0x34
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[Size] = Some(Size(MemSize.`32`, Extension.SX))
      override implicit val selfTag: ClassTag[I64Mem32SX] = ClassTag(classOf[I64Mem32SX])
    } with Companion[I64Mem32SX]

    case class I64Mem32ZX(
      override val align: Int,
      override val offset: UInt
    ) extends Load[I64Mem32ZX](I64Mem32ZX)

    object I64Mem32ZX extends {
      override val Header: Byte = 0x35
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[Size] = Some(Size(MemSize.`32`, Extension.ZX))
      override implicit val selfTag: ClassTag[I64Mem32ZX] = ClassTag(classOf[I64Mem32ZX])
    } with Companion[I64Mem32ZX]

  }

  abstract class Store[Self <: Store[Self]](companion: Store.Companion[Self]) extends MemoryOperation[MemSize] {
    override val memoryType: ValueType = companion.memoryType
    override val size: Option[MemSize] = companion.size
  }

  object Store {

    abstract class Companion[Self <: Store[Self]] extends MemoryOperation.Companion[Self, MemSize] {
      override val SName: String = {
        memoryType.sExprPart + "." + "store" +
          size.map(_.sExprPart).getOrElse("")
      }
    }

    case class I32None(
      override val align: Int,
      override val offset: UInt
    ) extends Store[I32None](I32None)

    object I32None extends {
      override val Header: Byte = 0x36
      override val memoryType: ValueType = ValueType.I32
      override val size: Option[MemSize] = None
      override implicit val selfTag: ClassTag[I32None] = ClassTag(classOf[I32None])
    } with Companion[I32None]

    case class I64None(
      override val align: Int,
      override val offset: UInt
    ) extends Store[I64None](I64None)

    object I64None extends {
      override val Header: Byte = 0x37
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[MemSize] = None
      override implicit val selfTag: ClassTag[I64None] = ClassTag(classOf[I64None])
    } with Companion[I64None]

    case class F32None(
      override val align: Int,
      override val offset: UInt
    ) extends Store[F32None](F32None)

    object F32None extends {
      override val Header: Byte = 0x38
      override val memoryType: ValueType = ValueType.F32
      override val size: Option[MemSize] = None
      override implicit val selfTag: ClassTag[F32None] = ClassTag(classOf[F32None])
    } with Companion[F32None]

    case class F64None(
      override val align: Int,
      override val offset: UInt
    ) extends Store[F64None](F64None)

    object F64None extends {
      override val Header: Byte = 0x39
      override val memoryType: ValueType = ValueType.F64
      override val size: Option[MemSize] = None
      override implicit val selfTag: ClassTag[F64None] = ClassTag(classOf[F64None])
    } with Companion[F64None]

    case class I32Mem8(
      override val align: Int,
      override val offset: UInt
    ) extends Store[I32Mem8](I32Mem8)

    object I32Mem8 extends {
      override val Header: Byte = 0x3a
      override val memoryType: ValueType = ValueType.I32
      override val size: Option[MemSize] = Some(MemSize.`8`)
      override implicit val selfTag: ClassTag[I32Mem8] = ClassTag(classOf[I32Mem8])
    } with Companion[I32Mem8]

    case class I32Mem16(
      override val align: Int,
      override val offset: UInt
    ) extends Store[I32Mem16](I32Mem16)

    object I32Mem16 extends {
      override val Header: Byte = 0x3b
      override val memoryType: ValueType = ValueType.I32
      override val size: Option[MemSize] = Some(MemSize.`16`)
      override implicit val selfTag: ClassTag[I32Mem16] = ClassTag(classOf[I32Mem16])
    } with Companion[I32Mem16]

    case class I64Mem8(
      override val align: Int,
      override val offset: UInt
    ) extends Store[I64Mem8](I64Mem8)

    object I64Mem8 extends {
      override val Header: Byte = 0x3c
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[MemSize] = Some(MemSize.`8`)
      override implicit val selfTag: ClassTag[I64Mem8] = ClassTag(classOf[I64Mem8])
    } with Companion[I64Mem8]

    case class I64Mem16(
      override val align: Int,
      override val offset: UInt
    ) extends Store[I64Mem16](I64Mem16)

    object I64Mem16 extends {
      override val Header: Byte = 0x3d
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[MemSize] = Some(MemSize.`16`)
      override implicit val selfTag: ClassTag[I64Mem16] = ClassTag(classOf[I64Mem16])
    } with Companion[I64Mem16]

    case class I64Mem32(
      override val align: Int,
      override val offset: UInt
    ) extends Store[I64Mem32](I64Mem32)

    object I64Mem32 extends {
      override val Header: Byte = 0x3e
      override val memoryType: ValueType = ValueType.I64
      override val size: Option[MemSize] = Some(MemSize.`32`)
      override implicit val selfTag: ClassTag[I64Mem32] = ClassTag(classOf[I64Mem32])
    } with Companion[I64Mem32]

  }

}

case object CurrentMemory extends {
  override val Header: Byte = 0x3f
  override val headerCodec: Codec[Unit] = codecs.constant(ByteVector(Header, 0x00))
} with Instruction.Singleton

case object GrowMemory extends {
  override val Header: Byte = 0x40
  override val headerCodec: Codec[Unit] = codecs.constant(ByteVector(Header, 0x00))
} with Instruction.Singleton

trait Const[A] extends Instruction {
  val value: A
}

object Const {

  case class I32(override val value: Int) extends Const[Int]

  object I32 extends Instruction.Companion {
    override type I = I32
    override val Header: Byte = 0x41
    override implicit val codec: Codec[I32] =
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
    override implicit val codec: Codec[I64] =
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
    override implicit val codec: Codec[F32] =
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
    override implicit val codec: Codec[F64] =
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
      Rotr
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
      GeU
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
      ReinterpretFloat
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
      Sqrt
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
      CopySign
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
      Gt
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
      ReinterpretInt
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
