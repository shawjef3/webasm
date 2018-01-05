package me.jeffshaw.webasm.ast

import fastparse.all.{End => InputEnd, _}
import fastparse.parsers.Combinators.Not
import me.jeffshaw.unsigned.UInt
import scala.annotation.tailrec

sealed trait Sexpr {
  def ++(that: Sexpr): Sexpr

  def values: Vector[Sexpr]

  def decode[A](implicit codec: Sexpr.Codec[A]): A =
    codec.decode(this)

  def maybeDecode[A](implicit codec: Sexpr.Codec.Partial[A]): Option[A] =
    codec.decoder.lift(this)
}

object Sexpr {

  case class Atom(value: String) extends Sexpr {
    override def toString: String = value

    override def ++(that: Sexpr): Sexpr = {
      that match {
        case Node(Vector()) =>
          this
        case Node(thoseValues) =>
          Node(values ++ thoseValues)
        case a: Atom =>
          Node(values :+ a)
      }
    }

    override lazy val values: Vector[Sexpr] =
      Vector(this)
  }

  case class Node(override val values: Vector[Sexpr]) extends Sexpr {
    override lazy val toString: String =
      values.mkString("(", " ", ")")

    override def ++(that: Sexpr): Sexpr = {
      that match {
        case Node(otherValues) =>
          Node(values ++ otherValues)
        case a: Atom =>
          Node(values :+ a)
      }
    }
  }

  object Node {
    def apply(children: Sexpr*): Node =
      if (children.isEmpty)
        empty
      else Node(children.toVector)

    val empty = Node()
  }

  /**
    * For creating and matching an Sexpr with one node.
    */
  object Singleton {
    def apply(singleton: Sexpr): Node = {
      Node(Vector(singleton))
    }

    /**
      * For matching an `Atom` that is on its own, or the only
      * value in a `Node`.
      * It matches any level of `Node` embedding. For instance,
      * it will find the `Atom` in `Node(Vector(Atom))`, or
      * `Node(Vector(Node(Vector(Atom))))`.
      */
    @tailrec
    def unapply(s: Sexpr): Option[Atom] = {
      s match {
        case a: Atom =>
          Some(a)
        case Node(Vector(maybeAtom)) =>
          unapply(maybeAtom)
        case _ =>
          None
      }
    }
  }

  /**
    * Even though we aren't representing s-expressions with linked lists, we can simulate cons.
    */
  object Cons {
    def apply(head: Sexpr, tail: Sexpr): Sexpr = {
      (head, tail) match {
        case (headAtom: Atom, tailAtom: Atom) =>
          Node(headAtom, tailAtom)
        case (headNode: Node, tailAtom: Atom) =>
          Node(Vector(headNode, tailAtom))
        case (headAtom: Atom, tailNode: Node) =>
          Node(headAtom +: tailNode.values)
        case (headNode: Node, tailNode: Node) =>
          Node(headNode +: tailNode.values)
      }
    }

    def unapply(s: Sexpr): Option[(Sexpr, Sexpr)] = {
      s match {
        case Sexpr.Node(head +: Vector(snoc)) =>
          Some((head, snoc))
        case Sexpr.Node(head +: tails) =>
          Some((head, Node(tails)))
        case _ =>
          None
      }
    }
  }

  trait Part {
    def sExprPart: String
  }

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
                Sexpr.Node(Atom(head) +: tails.toVector)
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

  trait Codec[A] {
    outer =>
    def encode(value: A): Sexpr
    def decode(s: Sexpr): A

    def xmap[B](f: A => B, g: B => A): Codec[B] =
      new Codec[B] {
        override def encode(value: B): Sexpr = outer.encode(g(value))

        override def decode(s: Sexpr): B = f(outer.decode(s))
      }
  }

  object Codec {
    def apply[A](implicit c: Codec[A]): Codec[A] = c

    trait Partial[A] extends Codec[A] {
      outer =>

      override def encode(value: A): Sexpr =
        encoder(value)

      override def decode(s: Sexpr): A =
        decoder(s)

      val encoder: PartialFunction[A, Sexpr]
      val decoder: PartialFunction[Sexpr, A]

      def orElse(that: Partial[A]): Partial[A] =
        new Partial[A] {
          override val encoder: PartialFunction[A, Sexpr] =
            outer.encoder.orElse(that.encoder)
          override val decoder: PartialFunction[Sexpr, A] =
            outer.decoder.orElse(that.decoder)
        }

      def lifted(default: => Sexpr): Codec[Option[A]] =
        new Codec[Option[A]] {
          override def encode(value: Option[A]): Sexpr =
            value.flatMap(outer.encoder.lift).getOrElse(default)

          override def decode(s: Sexpr): Option[A] =
            outer.decoder.lift(s)
        }

      def pxmap[B](f: A => B, g: PartialFunction[B, A]): Partial[B] =
        new Partial[B] {
          override val encoder: PartialFunction[B, Sexpr] =
            g.andThen(outer.encoder)

          override val decoder: PartialFunction[Sexpr, B] =
            outer.decoder.andThen(f)
        }
    }

    object Partial {
      def orElse[A](partials: Seq[Partial[A]]): Partial[A] =
        new Partial[A] {
          override val encoder: PartialFunction[A, Sexpr] =
            partials.foldLeft(PartialFunction.empty[A, Sexpr])(_ orElse _.encoder)
          override val decoder: PartialFunction[Sexpr, A] =
            partials.foldLeft(PartialFunction.empty[Sexpr, A])(_ orElse _.decoder)
        }
    }

    case class Maps[A](
      to: Map[A, Sexpr],
      from: Map[Sexpr, A]
    ) extends Codec[A] {
      override def encode(value: A): Sexpr = to(value)

      override def decode(s: Sexpr): A = from(s)
    }

    object Maps {
      def symmetric[A](to: Map[A, Sexpr]): Maps[A] =
        Maps(
          to = to,
          from = to.map(_.swap)
        )
    }
  }

  object Utils {
    implicit val int: Codec[Int] =
      new Codec[Int] {
        override def encode(value: Int): Sexpr =
          Codec[UInt].encode(UInt(value))

        override def decode(s: Sexpr): Int =
          Codec[UInt].decode(s).toInt
      }

    implicit val int32: Codec[UInt] =
      new Codec[UInt] {
        override def encode(value: Var): Sexpr =
          Atom(value.toString)

        override def decode(s: Sexpr): Var =
          s match {
            case Atom(value) =>
              UInt.valueOf(value)
            case _ => ???
          }
      }

    def stringWith(s: String, trans: Int => Seq[Int]): String = {
      val codePoints = s.codePoints.toArray
      codePoints.flatMap(trans).flatMap(java.lang.Character.toChars(_)).mkString("\"", "", "\"")
    }

    val bytesToEscape =
      Set('\n', '\t', '"', '\\').map(_.toInt)

    def hexChar(c: Int): Seq[Int] =
      ("\\" + c.formatted("%02x")).codePoints.toArray

    def bytes(s: String): String =
      stringWith(s, hexChar)

    def string(s: String): String = {
      stringWith(s,
        {
          case c if bytesToEscape.contains(c) => Seq('\\'.toInt, c)
          case c if 0x20 <= c && c < 0x7f =>
            Seq(c)
          case c =>
            hexChar(c)
        }
      )
    }

    def name(s: String): String =
      stringWith(s,
        {
          case c if (0x20 <= c && c < 0x7f)
            || c == 0x9 || c == 0xa =>
            Seq(c)
          case c =>
            Seq('\\'.toInt, 'u'.toInt, '{'.toInt) ++
              c.formatted("%x02").map(_.toInt)
        }
      )

    def tab[A](head: String, xs: Vector[A])(implicit codec: Codec[A]): Sexpr =
      Node(
        if (xs.isEmpty)
          Vector.empty
        else Atom(head) +: xs.map(codec.encode)
      )

    def limits[A](f: A => Sexpr, limits: Limits[A]): Sexpr =
      Node(f(limits.min) +: limits.max.map(f).toVector)

  }

  trait Syntax {
    implicit def vectorToVectorSyntax(v: Vector[Sexpr]): VectorSyntax =
      new VectorSyntax(v)

    implicit def stringToStringSyntax(s: String): StringSyntax =
      new StringSyntax(s)

    implicit def anyToSexprSyntax[A](a: A): ToSexprSyntax[A] =
      new ToSexprSyntax[A](a)
  }

  object Syntax extends Syntax

  class VectorSyntax(val v: Vector[Sexpr]) extends AnyVal {
    def node: Sexpr = Node(v)
  }

  class StringSyntax(val s: String) extends AnyVal {
    def atom: Sexpr = Atom(s)
    def node: Sexpr = Node(Atom(s))
  }

  class ToSexprSyntax[A](val a: A) extends AnyVal {
    def toSexpr(implicit codec: Codec[A]): Sexpr = codec.encode(a)
  }

}
