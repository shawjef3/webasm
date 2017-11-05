package me.jeffshaw.webasm.ast

import fastparse.all.{End => InputEnd, _}
import fastparse.parsers.Combinators.Not

sealed trait Sexpr

object Sexpr {

  case class Atom(value: String) extends Sexpr

  case class Node(head: String, tails: Vector[Sexpr] = Vector.empty) extends Sexpr

  def parse(s: String): Sexpr = {
    Parsers.sexpr.parse(s) match {
      case f: Parsed.Failure =>
        throw new Exception(f.msg)
      case Parsed.Success(value, _) =>
        value
    }
  }

  object Parsers {
    val hexDigit =
      P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))

    val stringElem =
      P(
        ("\\" ~ (
          CharIn("tnr\"'\\") |
            hexDigit.rep(exactly = 2) |
            "u{" ~ hexDigit.rep(min = 1) ~ "}"
          )
        ) |
          CharIn(
            " !",
            '#' to '[',
            ']' to '~',
            '\u0080' to Char.MaxValue
          )
      )

    val string =
      P("\"" ~ stringElem.rep ~ "\"")

    val keyword =
      P(
        CharIn(
          'a' to 'z',
          'A' to 'Z',
          '0' to '9',
          "!#$%&′*+-./:<=>?@\\^_`|~"
        ).rep(min = 1)
      )

    val atom: P[Sexpr] =
      P(string | keyword).!.map(Sexpr.Atom(_))

    val list: P[(String, Seq[Sexpr])] =
      P(
        "(" ~ Whitespace ~ keyword.! ~
          innerSexpr.rep ~ Whitespace ~ ")"
      )

    val innerSexpr: P[Sexpr] =
      P(
        Whitespace ~ (
          atom |
            list.map {
              case (head, tails) =>
                Sexpr.Node(head, tails.toVector)
            }
        )
      )

    val sexpr: P[Sexpr] =
      P(innerSexpr ~ Whitespace ~ InputEnd)

    val whitespaceChar =
      P(CharIn(" \n\r\t"))

    val Whitespace: P0 =
      P((Comment | whitespaceChar).rep)

    val Comment: P0 =
      P(BlockComment | LineComment)

    val LineComment: P0 =
      P(";;" ~ (Not("\n") ~ AnyChar).rep ~ ("\n" | InputEnd))

    val BlockComment: P0 =
      P("(;" ~ (
        Not(CharIn(";(")) ~ AnyChar |
          ";" ~ !")" |
          "(" ~ !";" |
          BlockComment
      ).rep ~ ";)")

  }

}
