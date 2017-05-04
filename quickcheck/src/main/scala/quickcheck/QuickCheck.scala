package quickcheck

import common._

import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
        v <- arbitrary[Int]
        h <- oneOf(const(empty), genHeap)
      } yield insert(v, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("minOfTwo") = forAll {
    (a: Int, b: Int) =>
      val g = insert(a, empty)
      val h = insert(b, g)
      findMin(h) == math.min(a, b)
  }

  property("delete min") = forAll {
    a: Int =>
      val g = insert(a, empty)
      val h = deleteMin(g)
      isEmpty(h)
  }

  /*property("order is preserved") = forAll {h: H =>
    (!isEmpty(h)) ==> {
      def orderChecker(v: Int, h: H): Boolean =
        if (!isEmpty(h)) v <= findMin(h) && orderChecker(findMin(h), deleteMin(h))
        else true

      orderChecker(findMin(h), deleteMin(h))
    }
  }*/

  property("minimum of melding 1") = forAll {
    (v1: Int, v2: Int) =>
      val minVal = math.min(v1, v2)
      val maxVal = math.max(v1, v2)

      val h1 = insert(v1, empty)
      val h2 = insert(v2, h1)

      findMin(h2) == minVal

      val h3 = deleteMin(h2)

      findMin(h3) == maxVal
  }

  property("minimum of melding 2") = forAll {
    (a: Int, b: Int, c: Int) =>
      val sorted = List(a, b, c).sorted
      val v1 = sorted.head
      val v2 = sorted.tail.head
      val v3 = sorted.tail.tail.head

      val h1 = insert(c, insert(b, insert(a, empty)))
      findMin(h1) == v1

      val h2 = deleteMin(h1)
      findMin(h2) == v2

      val h3 = deleteMin(h2)
      findMin(h3) == v3
  }
}