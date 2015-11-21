package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    math.min(a, b) == findMin(h)
  }

  property("delete min from single element heap") = forAll { a: Int =>
    val h = insert(a, empty)
    empty == deleteMin(h)
  }

  property("heapsort") = forAll { h: H =>
    @tailrec
    def help(h: H, xs: List[A]): List[A] = h match {
      case _ if isEmpty(h) => xs
      case _ => help(deleteMin(h), findMin(h) :: xs)
    }
    val xs = help(h, Nil)
    xs.sorted(ord.reverse) == xs
  }

  property("min of two") = forAll { (h1: H, h2: H) =>
    val min = findMin(meld(h1, h2))
    min == findMin(h1) || min == findMin(h2)
  }

  lazy val genHeap: Gen[H] = for {
    e <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(e, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
