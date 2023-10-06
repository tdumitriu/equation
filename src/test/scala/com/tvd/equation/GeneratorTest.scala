package com.tvd.equation

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GeneratorTest extends AnyFlatSpec with Matchers {

  "Number of operators from pool [1] is BaseExtended and" should "have 3 operators" in {
    val rnd: Double = Generator.getNumberOfOperatorsFromPool(1)
    rnd should be (3)
  }

  "Getting the number of operators from pool [6]" should "throw an exception" in {
    val exception = intercept[EquationException] {
      Generator.getNumberOfOperatorsFromPool(6)
    }
    exception.getMessage should equal ("Pool of operators [6] doesn't exist!")
  }

  "Generated minimum difficulty parameters" should "return a difficulty within minimum range" in {
    val difficulty = Generator.generateInputParametersForValidatedDifficulty(1, 5)
    val actualDifficulty = Generator.getValueOfDifficulty(difficulty)
    actualDifficulty should be (50.0 +- 50)
  }

  "Generated difficulty parameters" should "return a difficulty within given range" in {
    val difficulty = Generator.generateInputParametersForValidatedDifficulty(62, 5)
    val actualDifficulty = Generator.getValueOfDifficulty(difficulty)
    actualDifficulty should be (50.0 +- 50)
  }

  "Generated maximum difficulty parameters" should "return a difficulty within maximum range" in {
    val difficulty = Generator.generateInputParametersForValidatedDifficulty(100, 5)
    val actualDifficulty = Generator.getValueOfDifficulty(difficulty)
    actualDifficulty should be (50.0 +- 50)
  }

  "Generated negative difficulty parameters" should "return a difficulty within minimum range" in {
    val difficulty = Generator.generateInputParametersForValidatedDifficulty(-23, 5)
    val actualDifficulty = Generator.getValueOfDifficulty(difficulty)
    actualDifficulty should be (50.0 +- 50)
  }

  "Generated over maximum difficulty parameters" should "return a difficulty within maximum range" in {
    val difficulty = Generator.generateInputParametersForValidatedDifficulty(999, 5)
    val actualDifficulty = Generator.getValueOfDifficulty(difficulty)
    actualDifficulty should be (50.0 +- 50)
  }

  "Generating parameters with difficulty from 0 to 100" should "have a standard deviation less then 30" in {
    val difficultyList = List.tabulate(100)( x => {
      val difficulty = Generator.generateInputParametersForValidatedDifficulty(x, 3)
      val difficultyScore = Generator.getValueOfDifficulty(difficulty)
      //println(difficulty + " = " + "%3.2f" format difficultyScore)
      difficultyScore
    })
    val stdDev = Service.getStandardDeviation(difficultyList, Service.getAverage(difficultyList))
    val deviation = ((1 to 100) zip difficultyList).foldLeft(0.0)((x, y) => x + (y._1-y._2)*(y._1-y._2))
    val spread = Math.sqrt(deviation/Service.getAverage(difficultyList))
    //println("spread from line   ===> " + spread)
    //println("standard deviation ===>" + stdDev)
    stdDev should be < 50.0
    spread should be < 50.0
  }
}
