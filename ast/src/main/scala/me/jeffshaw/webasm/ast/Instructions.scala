package me.jeffshaw.webasm.ast

import scala.reflect.ClassTag
import scodec.{Attempt, Codec, Decoder, Encoder}
import scodec.bits.BitVector

case class Instructions(instructions: Vector[Instruction])

object Instructions {

  val bodyEncoder: Encoder[Vector[Instruction]] =
    Encoder { instructions =>
      instructions.foldLeft(Attempt.successful(BitVector.empty)) {
        case (accum, i) =>
          for {
            encodedAccum <- accum
            encoded <- Codec[Instruction].encode(i)
          } yield encodedAccum ++ encoded
      }
    }

  def codec[End](implicit endCodec: Codec[End], ctag: ClassTag[End]): Codec[(Instructions, End)] = {
    Codec[(Instructions, End)](
      encoder = Encoder[(Instructions, End)] { iEnd: (Instructions, End) =>
        val (Instructions(instructions), end) = iEnd
        for {
          bodyBits <- bodyEncoder.encode(instructions)
          endBits <- Codec[End].encode(end)
        } yield bodyBits ++ endBits
      },
      decoder = {
        def inner(accum: Vector[Instruction]): Decoder[(Vector[Instruction], End)] =
          for {
            head <- Codec[Instruction]
            tail <-
            head match {
              case end: End =>
                Decoder.point((accum, end))
              case _ =>
                inner(accum :+ head)
            }
          } yield tail

        inner(Vector.empty) xmap[(Instructions, End)](
          {
            case (instructions, end) => (Instructions(instructions), end)
          },
          { iEnd: (Instructions, End) =>
            iEnd.copy(_1 = iEnd._1.instructions)
          }
        )
      }
    )
  }

  val thenEnd: Codec[Instructions] =
    codec[End.type] xmap(
      _._1,
      i => (i, End)
    )

  implicit val sCodec: Sexpr.Codec[Instructions] =
    new Sexpr.Codec[Instructions] {
      override def encode(value: Instructions): Sexpr =
        Sexpr.Node(value.instructions.map(Sexpr.Codec[Instruction].encode))

      override def decode(s: Sexpr): Instructions =
        Instructions(s.values.map(Sexpr.Codec[Instruction].decode))
    }

}
