package com.tvd.equation

import com.tvd.equation.EquationConstant.UnaryOperator._

trait Tree {
  def value: Int
  def text: String
}

case class Branch(unaryOperator: String = None,
                  binaryOperator: String,
                  left: Tree,
                  right: Tree) extends Tree {

  def value: Int = Operator.binary(unaryOperator, binaryOperator, left.value, right.value)

  def text = "(" + left.text + binaryOperator + right.text + ")"
}

case class Leaf(unaryOperator: String = None,
                x: Int = 0) extends Tree {

  def value: Int = Operator.unary(unaryOperator, x)

  def text = unaryOperator match {
    case Square => x + unaryOperator
    case Negate => "(-" + x + ")"
    case _ => unaryOperator + x
  }
}
