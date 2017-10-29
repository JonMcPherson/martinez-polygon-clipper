package com.soulfiresoft.geometry.martinez

import org.scalatest.FlatSpec

import scala.collection.mutable

/**
  * Created by Jon on 10/9/2017.
  */
class CompareEventsTest extends FlatSpec {

  "Event queue" should "process lest(by x) sweep event first" in {
    val queue = new mutable.PriorityQueue[SweepEvent]()(Ordering[SweepEvent].reverse)
    val e1 = SweepEvent(Point(0, 0))
    val e2 = SweepEvent(Point(0.5, 0.5))

    queue += e1
    queue += e2

    assert(e1 === queue.dequeue())
    assert(e2 === queue.dequeue())
  }

  it should "process lest(by y) sweep event first" in {
    val queue = new mutable.PriorityQueue[SweepEvent]()(Ordering[SweepEvent].reverse)
    val e1 = SweepEvent(Point(0, 0))
    val e2 = SweepEvent(Point(0, 0.5))

    queue += e1
    queue += e2

    assert(e1 === queue.dequeue())
    assert(e2 === queue.dequeue())
  }

  it should "dequeue least(by left prop) sweep event first" in {
    val queue = new mutable.PriorityQueue[SweepEvent]()(Ordering[SweepEvent].reverse)
    val e1 = SweepEvent(Point(0, 0), left = true)
    val e2 = SweepEvent(Point(0, 0))

    queue += e1
    queue += e2

    assert(e2 === queue.dequeue())
    assert(e1 === queue.dequeue())
  }


  "Sweep event" should "compare x coordinates appropriately" in {
    val e1 = SweepEvent(Point(0, 0))
    val e2 = SweepEvent(Point(0.5, 0.5))

    assert(e1 < e2)
    assert(e2 > e1)
  }

  it should "compare y coordinates appropriately" in {
    val e1 = SweepEvent(Point(0, 0))
    val e2 = SweepEvent(Point(0, 0.5))

    assert(e1 < e2)
    assert(e2 > e1)
  }

  it should "compare left first" in {
    val e1 = SweepEvent(Point(0, 0), left = true)
    val e2 = SweepEvent(Point(0, 0))

    assert(e1 > e2)
    assert(e2 < e1)
  }

  it should "compare shared start point not collinear edges" in {
    val e1 = SweepEvent(Point(0, 0), SweepEvent(Point(1, 1)), left = true)
    val e2 = SweepEvent(Point(0, 0), SweepEvent(Point(2, 3)), left = true)

    assert(e1 < e2, "lower is processed first")
    assert(e2 > e1, "higher is processed second")
  }

  it should "compare collinear edges" in {
    val e1 = SweepEvent(Point(0, 0), SweepEvent(Point(1, 1), left = true), left = true, isSubject = true)
    val e2 = SweepEvent(Point(0, 0), SweepEvent(Point(2, 2), left = true), left = true)

    assert(e1 < e2, "clipping is processed first")
    assert(e2 > e1, "subject is processed second")
  }

}
