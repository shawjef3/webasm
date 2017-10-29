package me.jeffshaw.webasm.ast

import scodec.bits.ByteVector
import scodec.{Codec, codecs}

case class Import(
  moduleName: ByteVector, //string
  itemName: ByteVector, //string
  itemKind: Import.Kind
)

object Import {
  sealed trait Kind

  object Kind {
    case class Func(item: Var) extends Kind

    object Func {
      val id: Byte = 0

      val codec: Codec[Kind] =
        codecs.constant(ByteVector(id)) ~ wcodecs.vu32 xmap (
          {
            case ((), item) => Func(item)
          },
          {
            case Func(item) => ((), item)
          }
        )
    }

    case class Table(tableType: TableType) extends Kind

    object Table {
      val id: Byte = 1

      val codec: Codec[Kind] =
        codecs.constant(ByteVector(id)) ~ Codec[TableType] xmap(
          {
            case ((), tableType) => Table(tableType)
          },
          {
            case Table(tableType) => ((), tableType)
          }
        )
    }

    case class Memory(memoryType: MemoryType) extends Kind

    object Memory {
      val id: Byte = 2

      val codec: Codec[Kind] =
        codecs.constant(ByteVector(id)) ~ Codec[MemoryType] xmap(
          {
            case ((), memoryType) => Memory(memoryType)
          },
          {
            case Memory(memoryType) => ((), memoryType)
          }
        )
    }

    case class Global(globalType: GlobalType) extends Kind

    object Global {
      val id: Byte = 3

      val codec: Codec[Kind] =
        codecs.constant(ByteVector(id)) ~ Codec[GlobalType] xmap(
          {
            case ((), globalType) => Global(globalType)
          },
          {
            case Global(globalType) => ((), globalType)
          }
        )
    }

    implicit val codec: Codec[Kind] =
      codecs.choice(Func.codec, Table.codec, Memory.codec, Global.codec)

  }

  implicit val codec: Codec[Import] =
    wcodecs.string ~ wcodecs.string ~ Codec[Kind] xmap(
      {
        case ((moduleName, itemName), itemKind) => Import(moduleName, itemName, itemKind)
      },
      {
        case Import(moduleName, itemName, itemKind) => ((moduleName, itemName), itemKind)
      }
    )
}
