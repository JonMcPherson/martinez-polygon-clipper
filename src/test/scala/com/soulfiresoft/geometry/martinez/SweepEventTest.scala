package com.soulfiresoft.geometry.martinez

import org.scalatest.FlatSpec

class SweepEventTest extends FlatSpec {

  it should "be below" in {
    val se1 = SweepEvent(Point(0, 0), SweepEvent(Point(1, 1)), left = true)
    val se2 = SweepEvent(Point(0, 1), SweepEvent(Point(0, 0)))

    assert(se1.isBelow(Point(0, 1)))
    assert(se1.isBelow(Point(1, 2)))
    assert(!se1.isBelow(Point(0, 0)))
    assert(!se1.isBelow(Point(5, -1)))

    assert(!se2.isBelow(Point(0, 1)))
    assert(!se2.isBelow(Point(1, 2)))
    assert(!se2.isBelow(Point(0, 0)))
    assert(!se2.isBelow(Point(5, -1)))
  }

  it should "be above" in {
    val se1 = SweepEvent(Point(0, 0), SweepEvent(Point(1, 1)), left = true)
    val se2 = SweepEvent(Point(0, 1), SweepEvent(Point(0, 0)))

    assert(!se1.isAbove(Point(0, 1)))
    assert(!se1.isAbove(Point(1, 2)))
    assert(se1.isAbove(Point(0, 0)))
    assert(se1.isAbove(Point(5, -1)))

    assert(se2.isAbove(Point(0, 1)))
    assert(se2.isAbove(Point(1, 2)))
    assert(se2.isAbove(Point(0, 0)))
    assert(se2.isAbove(Point(5, -1)))
  }

  it should "be vertical" in {
    val se1 = SweepEvent(Point(0, 0), SweepEvent(Point(0, 1)), left = true)
    val se2 = SweepEvent(Point(0, 0), SweepEvent(Point(0.0001, 1)), left = true)

    assert(se1.isVertical())
    assert(!se2.isVertical())
  }

}
