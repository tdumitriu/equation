package com.tvd.equation

import com.tvd.equation.EquationConstant.UnaryOperator._
import com.tvd.equation.EquationConstant.BinaryOperator._

object Operator {

  def computeSafe(tree: Tree): (Int, Boolean) = {
    try { (compute(tree), true) }
    catch { case e: EquationException => (0, false) }
  }

  def compute(tree: Tree): Int = tree match {
    case Branch(unaryOperator, binaryOperator, left, right) =>
      Operator.binary(unaryOperator, binaryOperator, compute(left), compute(right))
    case Leaf(unaryOperator, argument) =>
      Operator.unary(unaryOperator, argument)
  }

  def arguments(tree: Tree): List[Int] = tree match {
    case Branch(unaryOperator, binaryOperator, left, right) =>
      arguments(left) ::: arguments(right)
    case Leaf(unaryOperator, argument) =>
      List(argument)
  }

  def operators(tree: Tree): List[String] = tree match {
    case Branch(unaryOperator, binaryOperator, left, right) =>
      List(binaryOperator) ::: operators(left) ::: operators(right)
    case Leaf(unaryOperator, argument) =>
      Nil
  }

  def binary(unaryOperator: String, binaryOperator: String, left: Int, right: Int): Int = binaryOperator match {
    case Addition => unary(unaryOperator, left + right)
    case Subtraction => unary(unaryOperator, left - right)
    case Multiplication => unary(unaryOperator, left * right)
    case Division =>
      if(right == 0) {
        throw new EquationException("Division by zero is not allowed !")
      } else {
        val div = left / right
        if(left == div * right) {
          unary(unaryOperator, div)
        } else {
          throw new EquationException("Division that doesn't return an integer is not allowed !")
        }
      }
    case Power =>
      if(left == 0 && right == 0) {
        throw new EquationException("Zero to the power of zero is undefined since x^y as a function of 2 variables is not continuous at the origin !")
      } else if(left == 0) {
        0
      } else if(right == 0) {
        1
      } else if(left >= 0 && right < 0) {
        binary(None, Division, 1, binary(None, Power, left, (-1)*right))
      } else {
        val result = Math.pow(left, right)
        val maxResult = Int.MaxValue
        if(result <  maxResult) {
          unary(unaryOperator, result.toInt)
        } else {
          throw new EquationException("Result exceeds maximum limit !")
        }
      }
    case Modulo =>
      if(left == 0 && right == 0) {
        throw new EquationException("Modulo of zero is undefined !")
      } else if(left == 0) {
        0
      } else if(right == 0) {
        throw new EquationException("Modulo of zero is undefined !")
      } else {
        unary(unaryOperator, left % right)
      }
  }

  def unary(unaryOperator: String, argument: Int): Int = unaryOperator match {
    case None => argument
    case Negate => (-1) * argument
    case Factorial => if(argument < 0) {
      throw new EquationException("Factorial of a negative number is not allowed !")
    } else {
      factorial(argument)
    }
    case Square => argument * argument
    case SquareRoot =>
      if(argument < 0) {
        throw new EquationException("Square root of a negative number is not allowed !")
      } else {
        val sqrt = Math.sqrt(argument).asInstanceOf[Int]
        if(argument == (sqrt * sqrt)) {
          sqrt.toInt
        } else {
          throw new EquationException("Square root that doesn't return an integer is not allowed !")
        }
      }
  }

  def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n-1)
}
