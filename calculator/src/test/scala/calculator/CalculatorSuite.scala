package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("delta") {
    val delta = Polynomial.computeDelta(Var(1), Var(-5), Var(6))
    assert(delta() == 1)
  }

  test("roots1") {
    val roots = Polynomial.computeSolutions(Var(1), Var(-5), Var(6), Var(1))
    assert(roots() == Set(2.0, 3.0))
  }

  test("roots2") {
    val roots = Polynomial.computeSolutions(Var(5), Var(-5), Var(6), Var(-95))
    assert(roots() == Set())
  }

  test("eval1") {
    val res = Calculator.eval(Times(Literal(2.0), Literal(3.0)), Map.empty)
    assert(res == 6)
  }

  test("eval2") {
    val res = Calculator.eval(Times(Ref("a"), Ref("b")), Map("a" -> Var(Literal(3.0)), "b" -> Var(Literal(2.0))))
    assert(res == 6)
  }

  test("eval3") {
    val res = Calculator.eval(Ref("a"), Map("a" -> Var(Plus(Ref("b"), Literal(3.0))), "b" -> Var(Plus(Ref("a"), Literal(3.0)))))
    assert(res.isNaN)
  }

  test("compute values") {
    val res = Calculator.computeValues(Map("a*b" -> Signal(Times(Ref("a"), Ref("b"))),
      "a" -> Var(Literal(4.0)),
      "b" -> Var(Literal(5.0))))

    assert(res("a*b")() == 20.0)
    assert(res("a")() == 4.0)
    assert(res("b")() == 5.0)
  }

  test("self reference") {
    val res = Calculator.computeValues(Map("a*b" -> Signal(Times(Ref("a"), Ref("b"))),
      "a" -> Var(Literal(4.0)),
      "b" -> Var(Literal(5.0))))

    assert(res("a*b")() == 20.0)
    assert(res("a")() == 4.0)
    assert(res("b")() == 5.0)
  }
}
