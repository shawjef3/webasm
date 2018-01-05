package me.jeffshaw.webasm.ast

import scodec.Codec

case class GlobalType(
  valueType: ValueType,
  mutability: Mutability
)

object GlobalType {
  implicit val codec: Codec[GlobalType] =
    ValueType.codec ~ Codec[Mutability] xmap[GlobalType](
      (GlobalType.apply _).tupled,
      (g: GlobalType) => (g.valueType, g.mutability)
    )

  implicit val sCodec: Sexpr.Codec[GlobalType] =
    new Sexpr.Codec[GlobalType] {
      override def encode(value: GlobalType): Sexpr =
        value match {
          case GlobalType(t, Immutable) =>
            Sexpr.Codec[ValueType].encode(t)
          case GlobalType(t, Mutable) =>
            Sexpr.Cons(Sexpr.Atom("mut"), Sexpr.Codec[ValueType].encode(t))
        }

      override def decode(s: Sexpr): GlobalType = {
        s match {
          case Sexpr.Singleton(atom) =>
            GlobalType(
              Sexpr.Codec[ValueType].decode(atom),
              Immutable
            )
          case Sexpr.Node(Vector(Sexpr.Atom("mut"), t)) =>
            GlobalType(
              Sexpr.Codec[ValueType].decode(t),
              Mutable
            )
        }
      }
    }

}
