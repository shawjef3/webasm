package me.jeffshaw.webasm.ast

case class Func(
  funcType: Var,
  locals: Vector[ValueType],
  body: Instructions
)
