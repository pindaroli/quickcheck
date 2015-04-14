package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
property("min2") = forAll { a: Int =>
    val h1=insert(a,empty)
    forAll { b: Int => 
      val  h2=insert(b,h1)
      if (a>b) b==findMin(h2) else a==findMin(h2)
    }
  }
  property("mindel") = forAll { a: Int =>
    val h=insert(a,empty)
    val h2=deleteMin(h)
    isEmpty(h2)
  }
  property("minMeld") = forAll { h1: H =>
    forAll { h2: H =>
      
      if (!isEmpty(h1) && !isEmpty(h2)) {
          val min=findMin(meld(h1,h2))

          min == findMin(h1) || min==findMin(h2)
      } else
        true
    }
  }
  property("ordered") = forAll{h: H =>
    val l=linearize(h)
    if (isOrdered(l)) true else false;
  }
 
 
  def isOrdered(l:List[Int]): Boolean = l match {
    case Nil => true  
    case x::xs => if (xs.isEmpty) true else x<=xs.head && isOrdered(xs) 
  }

  def linearize(h:H):List[Int] = { 
    if (isEmpty(h)) {List()}
    else {
      val min=findMin(h)
      min::linearize(deleteMin(h))
    }
  }
  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(const(empty),genHeap)
  } yield insert(i,h)



  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
