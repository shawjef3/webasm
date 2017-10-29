package me.jeffshaw.webasm.ast

import scodec.{Attempt, Codec, Decoder, Encoder, Err}

sealed trait Mutability {
  val isMutable: Boolean

  val Id: Byte
}

object Mutability {
  val values = Set(Immutable, Mutable)

  val idToValue = {
    for (value <- values) yield
      value.Id -> value
  }.toMap

  implicit val codec: Codec[Mutability] =
    Codec(
      encoder =
        Encoder { (m: Mutability) => wcodecs.u8.encode(m.Id)},
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

case object Immutable extends Mutability {
  override val isMutable: Boolean = false

  override val Id: Byte = 0

  implicit val codec: Codec[Immutable.type] =
    wcodecs.u8Const(0).xmap(
      _ => Immutable,
      _ => ()
    )
}

case object Mutable extends Mutability {
  override val isMutable: Boolean = true

  override val Id: Byte = 1

  implicit val codec: Codec[Mutable.type] =
    wcodecs.u8Const(1).xmap(
      _ => Mutable,
      _ => ()
    )
}
