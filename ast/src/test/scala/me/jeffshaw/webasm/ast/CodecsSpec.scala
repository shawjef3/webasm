package me.jeffshaw.webasm.ast

import me.jeffshaw.unsigned.ULong
import me.jeffshaw.webasm.ast.nodes.wcodecs
import org.scalatest.FunSuite
import scodec.Attempt
import scodec.bits.BitVector

class CodecsSpec extends FunSuite {

  test("vu64") {
    val values =
      Vector(
        (ULong(0L), BitVector(0x00)),
        (ULong(624485L), BitVector(0xE5, 0x8E, 0x26)),
        (ULong.MaxValue, BitVector(0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01))
      )

    for ((expectedLong, expectedBits) <- values) {
      val actualEncoded = wcodecs.vu64.encode(expectedLong)
      assertResult(Attempt.successful(expectedBits))(actualEncoded)

      val actualDecoded = wcodecs.vu64.decodeValue(expectedBits)
      assertResult(Attempt.successful(expectedLong))(actualDecoded)
    }
  }

  test("vs64") {
    val values =
      Vector(
        (-1L, BitVector(0x7f)),
        (0L, BitVector(0x00)),
        (Long.MinValue, BitVector(0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x7f)),
        (Long.MaxValue, BitVector(0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00))
      )

    for ((expectedLong, expectedBits) <- values) {
      val actualEncoded = wcodecs.vs64.encode(expectedLong)
      assertResult(Attempt.successful(expectedBits))(actualEncoded)

      val actualDecoded = wcodecs.vs64.decodeValue(expectedBits)
      assertResult(Attempt.successful(expectedLong))(actualDecoded)
    }
  }

}
