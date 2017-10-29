package me.jeffshaw.webasm.ast

import scodec.{Attempt, Codec, Decoder, Encoder, Err}

sealed trait ValueType {
  val Id: Byte
}

object ValueType {
  case object I32 extends ValueType {
    override val Id: Byte = 0x01
  }
  case object I64 extends ValueType {
    override val Id: Byte = 0x02
  }
  case object F32 extends ValueType {
    override val Id: Byte = 0x03
  }
  case object F64 extends ValueType {
    override val Id: Byte = 0x04
  }

  val values: Set[ValueType] =
    Set(I32, I64, F32, F64)

  val idToValue: Map[Byte, ValueType] = {
                                          for (value <- values) yield
                                            value.Id -> value
                                        }.toMap

  val codec: Codec[ValueType] =
    Codec(
      encoder =
        Encoder((m: ValueType) => wcodecs.u8.encode(m.Id)),
      decoder =
        Decoder {bits =>
          for {
            b <- wcodecs.u8.decode(bits)
            m <-
            idToValue.get(b.value) match {
              case Some(m) =>
                Attempt.successful(b.copy(value = m))
              case None =>
                Attempt.failure(Err(s"Expected one of ${values.map(_.Id)}."))
            }
          } yield m
        }
    )

}