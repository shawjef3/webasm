package me.jeffshaw.webasm.ast

import scodec.Codec

case class Table(
  tableType: TableType
)

object Table {
  implicit val codec: Codec[Table] =
    Codec[TableType].xmap(
      Table(_),
      _.tableType
    )
}
