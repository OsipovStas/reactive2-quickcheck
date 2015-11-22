package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop.{BooleanOperators, forAll}
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
    val xs: List[Int] = heapSort(h)
    xs.sorted(ord.reverse) == xs
  }

  lazy val genHeap: Gen[H] = for {
    e <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(e, h)

  property("min of two") = forAll { (h1: H, h2: H) =>
    val min = findMin(meld(h1, h2))
    min == findMin(h1) || min == findMin(h2)
  }

  property("find min in list") = forAll { xs: List[Int] =>
    xs.nonEmpty ==> {
      val heap: H = fromList(xs)
      xs.min == findMin(heap)
    }
  }
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("find min in two lists") = forAll { (xs: List[Int], ys: List[Int]) =>
    (xs.nonEmpty || ys.nonEmpty) ==> {
      (xs ++ ys).min == findMin(meld(fromList(xs), fromList(ys)))
    }
  }

  property("heap sort 2") = forAll { xs: List[Int] =>
    xs.sorted == heapSort(fromList(xs)).reverse
  }

  def heapSort(h: H): List[Int] = {
    @tailrec
    def help(h: H, xs: List[Int]): List[Int] = h match {
      case _ if isEmpty(h) => xs
      case _ => help(deleteMin(h), findMin(h) :: xs)
    }
    val xs = help(h, Nil)
    xs
  }

  def fromList(xs: List[Int]): H = xs.foldRight(empty) {
    case (e, h) => insert(e, h)
  }

}
