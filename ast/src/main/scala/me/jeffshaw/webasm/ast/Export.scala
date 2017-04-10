package me.jeffshaw.webasm.ast

import me.jeffshaw.webasm.ast.nodes.wcodecs
import scodec.bits.ByteVector
import scodec.{Codec, codecs}

case class Export(
  name: ByteVector,
  exportKind: Export.Kind,
  item: Var
)

object Export {
  sealed trait Kind {
    val id: Byte

    lazy val codec: Codec[Kind] =
      codecs.constant(ByteVector(id)).
      xmap(Function.const(this), Function.const(()))
  }

  object Kind {
    case object Func extends Kind {
      override val id: Byte = 0
    }
    case object Table extends Kind {
      override val id: Byte = 1
    }
    case object Memory extends Kind {
      override val id: Byte = 2
    }
    case object Global extends Kind {
      override val id: Byte = 3
    }

    implicit val codec: Codec[Kind] =
      codecs.choice(Func.codec, Table.codec, Memory.codec, Global.codec)
  }

  implicit val codec: Codec[Export] =
    wcodecs.string ~ Codec[Kind] ~ wcodecs.vu32 xmap(
      {
        case ((nameBytes, exportKind), item) =>
          Export(nameBytes, exportKind, item)
      },
      (e: Export) => ((e.name, e.exportKind), e.item)
    )
}
