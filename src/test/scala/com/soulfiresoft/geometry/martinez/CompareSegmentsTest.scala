package com.soulfiresoft.geometry.martinez

import com.soulfiresoft.collections.AVLTree
import org.scalatest.FlatSpec

class CompareSegmentsTest extends FlatSpec {

  "Non collinear segments" should "compare right point first with shared left points" in {
    val tree = new AVLTree[SweepEvent]()(SweepEvent.CompareSegments)
    val point = Point(0, 0)
    val se1 = SweepEvent(point, SweepEvent(Point(1, 1)), left = true)
    val se2 = SweepEvent(point, SweepEvent(Point(2, 3)), left = true)

    tree.insert(se1)
    tree.insert(se2)

    assert(tree.max.otherEvent.point == Point(2, 3))
    assert(tree.min.otherEvent.point == Point(1, 1))
  }

  it should "compare right point sorted on y coord with different left points" in {
    val tree = new AVLTree[SweepEvent]()(SweepEvent.CompareSegments)
    val se1 = SweepEvent(Point(0, 1), SweepEvent(Point(1, 1)), left = true)
    val se2 = SweepEvent(Point(0, 2), SweepEvent(Point(2, 3)), left = true)

    tree.insert(se1)
    tree.insert(se2)

    assert(tree.min.otherEvent.point == Point(1, 1))
    assert(tree.max.otherEvent.point == Point(2, 3))
  }

  it should "order events in sweep line" in {
    val se1 = SweepEvent(Point(0, 1), SweepEvent(Point(2, 1)), left = true)
    val se2 = SweepEvent(Point(-1, 0), SweepEvent(Point(2, 3)), left = true)

    val se3 = SweepEvent(Point(0, 1), SweepEvent(Point(3, 4)), left = true)
    val se4 = SweepEvent(Point(-1, 0), SweepEvent(Point(3, 1)), left = true)

    assert(se1 > se2)
    assert(!se2.isBelow(se1.point))
    assert(se2.isAbove(se1.point))

    assert((se1 <=> se2) == -1, "compare segments")
    assert((se2 <=> se1) == 1, "compare segments inverted")

    assert(se3 > se4)
    assert(!se4.isAbove(se3.point))
  }

  it should "compare first point below" in {
    val se1 = SweepEvent(Point(-1, 0), SweepEvent(Point(2, 3)), left = true)
    val se2 = SweepEvent(Point(0, 1), SweepEvent(Point(2, 1)), left = true)

    assert(!se1.isBelow(se2.point))
    assert((se1 <=> se2) == 1, "compare segments")
  }

  "Collinear segments" should "compare correctly" in {
    val se1 = SweepEvent(Point(1, 1), SweepEvent(Point(5, 1)), left = true, isSubject = true)
    val se2 = SweepEvent(Point(2, 1), SweepEvent(Point(3, 1)), left = true)

    assert(se1.isSubject != se2.isSubject)
    assert((se1 <=> se2) == -1)
  }

  it should "compare with shared left point" in {
    val pt = Point(0, 1)
    val se1 = SweepEvent(pt, SweepEvent(Point(5, 1)), left = true)
    val se2 = SweepEvent(pt, SweepEvent(Point(3, 1)), left = true)

    se1.contourId = 1
    se2.contourId = 2

    assert(se1.isSubject == se2.isSubject)
    assert(se1.point == se2.point)

    assert((se1 <=> se2) == -1)

    se1.contourId = 2
    se2.contourId = 1

    assert((se1 <=> se2) == 1)
  }

  it should "compare with same polygon and different left points" in {
    val se1 = SweepEvent(Point(1, 1), SweepEvent(Point(5, 1)), left = true, isSubject = true)
    val se2 = SweepEvent(Point(2, 1), SweepEvent(Point(3, 1)), left = true, isSubject = true)

    assert(se1.isSubject == se2.isSubject)
    assert(se1.point != se2.point)
    assert((se1 <=> se2) == -1)
    assert((se2 <=> se1) == 1)
  }

}
