package me.jeffshaw.webasm.ast

import fastparse.core.Parsed
import org.scalatest.FunSuite
import me.jeffshaw.webasm.ast.Sexpr._

class SexprSpec extends FunSuite {

  for ((toParse, expected) <- SexprSpec.goodSexprs) {
    test("sexpr " + toParse) {
      Parsers.sexpr.parse(toParse) match {
        case f: Parsed.Failure[Char, String] =>
          assert(false, f)
        case Parsed.Success(actual, _) =>
          assertResult(expected)(actual)
      }
    }
  }

  for (toParse <- SexprSpec.badSexprs) {
    test("sexpr " + toParse) {
      assert(Parsers.innerSexpr.parse(toParse).isInstanceOf[Parsed.Failure[_, _]])
    }
  }

  for (toParse <- SexprSpec.goodStrings) {
    test("string " + toParse) {
      Parsers.string.parse(toParse) match {
        case f: Parsed.Failure[Char, String] =>
          assert(false, f)
        case Parsed.Success(actual, _) =>
         assert(true)
      }
    }
  }

}

object SexprSpec {
  val goodSexprs: Map[String, Sexpr] =
    Map(
      "(x \"\")" -> Node("x", Vector(Atom("\"\""))),
      "(x)" -> Node("x"),
      "(x y)" -> Node("x", Vector(Atom("y"))),
      "x" -> Atom("x"),
      "(func (export \"\uD800\uDC00\uDB3F\uDFFF\uDBFF\uDFFF\") (result i32) (i32.const 44))" ->
        Node(
          "func",
          Vector(
            Node(
              "export",
              Vector(
                Atom("\"\uD800\uDC00\uDB3F\uDFFF\uDBFF\uDFFF\""),
              )
            ),
            Node("result", Vector(Atom("i32"))),
            Node("i32.const", Vector(Atom("44")))
          )
        ),
      "(module (func (export \"\uD800\uDC00\uDB3F\uDFFF\uDBFF\uDFFF\") (result i32) (i32.const 44)))" ->
        Node(
          "module",
          Vector(
            Node(
              "func",
              Vector(
                Node(
                  "export",
                  Vector(
                    Atom("\"\uD800\uDC00\uDB3F\uDFFF\uDBFF\uDFFF\""),
                  )
                ),
                Node("result", Vector(Atom("i32"))),
                Node("i32.const", Vector(Atom("44")))
              )
            )
          )
        ),
      "(x);;" -> Node("x"),
      ";;\n(x)" -> Node("x"),
      "(x)\n(;;)" -> Node("x"),
      "(;\n;)(x)" -> Node("x"),
      "(;\n(;;);)(x)" -> Node("x")
    )

  val badSexprs =
    Set(
      "()",
      "(",
      ")",
      "\"",
      ";;",
      ";",
      "(;",
      ";)",
      "(;;)",
      "(;\n(;;)(x)",
      "\n(;;);)(x)"
    )

  val goodStrings =
    Set(
      "\"\"",
      "\"\uD800\uDC00\uDB3F\uDFFF\uDBFF\uDFFF\""
    )
}
