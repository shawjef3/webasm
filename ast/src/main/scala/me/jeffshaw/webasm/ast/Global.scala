package me.jeffshaw.webasm.ast

import scodec.Codec
import shapeless.syntax.std.product._

case class Global(
  globalType: GlobalType,
  value: Instructions //const
)

object Global {
  implicit val codec: Codec[Global] =
    Codec[GlobalType] ~ Instructions.thenEnd xmap(
      (Global.apply _).tupled,
      _.toTuple
    )
}
