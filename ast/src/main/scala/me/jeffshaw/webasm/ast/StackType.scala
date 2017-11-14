package me.jeffshaw.webasm.ast

case class StackType(types: Vector[ValueType])

object StackType {

  implicit val codec: scodec.Codec[StackType] =
    wcodecs.vec(ValueType.codec) xmap[StackType](
      StackType(_),
      _.types
    )

  implicit val sCodec: Sexpr.Codec[StackType] =
    new Sexpr.Codec[StackType] {
      override def encode(value: StackType): Sexpr =
        Sexpr.Node(value.types.map(Sexpr.Codec[ValueType].encode))

      override def decode(s: Sexpr): StackType = {
        StackType(s.values.map(Sexpr.Codec[ValueType].decode))
      }
    }

}
