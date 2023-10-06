package com.tvd.equation

import com.tvd.equation.EquationConstant.{Parenthesis, BinaryOperator, ArgumentMaximumValue, ListOfOperators}

object Validate {

  def input(arguments: List[Int], operators: List[String]): Boolean = {
    if(arguments.isEmpty)
      throw new EquationException("At least two arguments for a binary operator must be provided!")
    if(arguments.nonEmpty && arguments.size == 1)
      throw new EquationException("At least two arguments for a binary operator must be provided!")
    if(operators.isEmpty)
      throw new EquationException("At least one binary operator must be provided!")
    if(arguments.size != (operators.size + 1))
      throw new EquationException("The number of arguments must be equal with the (number of operators + 1)!")
    if(notInRange(arguments))
      throw new EquationException("Arguments must be in [-" + ArgumentMaximumValue + ", " + ArgumentMaximumValue + "] range!")
    if(!definedOperator(operators))
      throw new EquationException("Undefined operator. Allowed operators: [" + ListOfOperators.toString + "]")
    true
  }

  def notInRange(arguments: List[Int]): Boolean = arguments.foldLeft(false)(_ || Math.abs(_) > ArgumentMaximumValue)

  def definedOperator(operators: List[String]): Boolean = operators.forall(ListOfOperators.contains(_))

  def validateEquation(value: String): List[Char] = {
    val trimmedValue = value.replaceAll(" ", "")
    trimmedValue.toList.map{
      x => if(!validCharacter(x))
        throw new EquationException("Invalid character [" + x + "] found in equation!")
    }
    if(!validateParenthesis(trimmedValue.toList))
      throw new EquationException("Parenthesises are not balanced!")
    trimmedValue.toList
  }

  def validCharacter(x: Char): Boolean = {
    if(BinaryOperator.list.contains(x.toString)) true
    else if(Parenthesis.list.contains(x.toString)) true
    else if(x.isDigit) true
    else false
  }

  def insertDelimiters(value: List[Char]): List[Char] = {
    var res = List[Char]()
    if(value.tail.head=='(') {
      var counter = 0
      var i = 0
      do {
        val index: Char = value.tail(i)
        if(index=='(') counter = counter + 1
        if(index==')') counter = counter - 1
        res = res :+ index
        i = i + 1
      } while(i < value.tail.length && counter > 0)
      res = res :+ '|' :+ value(i+1) :+ '|'
      res = res  ::: value.drop(i+2).init
    } else  {
      var i = 0
      var g = true
      do {
        val index: Char = value.tail(i)
        if(!index.isDigit) {
          g = false
        } else {
          res = res :+ index
          i = i + 1
        }
      } while(i < value.tail.length && g)
      if(res.isEmpty && ((value(i+1) == '+') || (value(i+1) == '-'))) res = res :+ '0'
      res = res :+ '|' :+ value(i+1) :+ '|'
      res = res  ::: value.drop(i+2).init
    }
    res
  }

  def injectDelimiters(value: List[Char]): List[Char] = value.tail.head match {
    case '(' =>
      null
    case x if x.isDigit =>
      null
    case x =>
      throw  new EquationException("Error parsing equation: Character ["+x+"] not expected!")
  }

  def validateParenthesis(chars: List[Char]): Boolean = chars.foldLeft(0){
    case (0, ')') => return false
    case (x, ')') => x - 1
    case (x, '(') => x + 1
    case (x, _  ) => x
  } == 0
}
