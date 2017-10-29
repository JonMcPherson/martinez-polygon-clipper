package com.soulfiresoft.geometry.martinez

import com.soulfiresoft.collections.AVLTree
import com.soulfiresoft.geometry.martinez.PolygonClipper._
import org.scalatest.FlatSpec

import scala.Double._
import scala.collection.mutable

class DivideSegmentsTest extends FlatSpec {

  private val (subject, clipping) = fixtures.loadGeoJsonSubjectClipping("/fixtures/two_shapes.json")

  it should "divide two segments" in {
    val se1 = SweepEvent(Point(0, 0), SweepEvent(Point(5, 5)), left = true, isSubject = true)
    val se2 = SweepEvent(Point(0, 5), SweepEvent(Point(5, 0)), left = true)
    val queue = new mutable.PriorityQueue[SweepEvent]()(Ordering[SweepEvent].reverse)

    queue += se1
    queue += se2

    val points = intersection(
      se1.point, se1.otherEvent.point,
      se2.point, se2.otherEvent.point
    )

    divideSegment(se1, points(0), queue)
    divideSegment(se2, points(0), queue)

    assert(queue.length == 6, "subdivided in 4 segments by intersection point")
  }

  it should "determine possible intersections" in {
    val subjectPolygons = fixtures.extractCoordinates(subject)
    val clippingPolygons = fixtures.extractCoordinates(clipping)

    val queue = new mutable.PriorityQueue[SweepEvent]()(Ordering[SweepEvent].reverse)

    val se1 = SweepEvent(subjectPolygons(0)(3), SweepEvent(subjectPolygons(0)(2)), left = true, isSubject = true)
    val se2 = SweepEvent(clippingPolygons(0)(0), SweepEvent(clippingPolygons(0)(1)), left = true)

    // console.log(se1.point, se1.left, se1.otherEvent.point, se1.otherEvent.left);
    // console.log(se2.point, se2.left, se2.otherEvent.point, se2.otherEvent.left);

    assert(possibleIntersection(se1, se2, queue) == 1)
    assert(queue.length == 4)

    val e1 = queue.dequeue()
    assert(e1.point == Point(100.79403384562251, 233.41363754101192))
    assert(e1.otherEvent.point == Point(56, 181))

    val e2 = queue.dequeue()
    assert(e2.point == Point(100.79403384562251, 233.41363754101192))
    assert(e2.otherEvent.point == Point(16, 282))

    val e3 = queue.dequeue()
    assert(e3.point == Point(100.79403384562251, 233.41363754101192))
    assert(e3.otherEvent.point == Point(153, 203.5))

    val e4 = queue.dequeue()
    assert(e4.point == Point(100.79403384562251, 233.41363754101192))
    assert(e4.otherEvent.point == Point(153, 294.5))
  }

  it should "subdivide segments on 2 polygons" in {
    val subjectPolygons = MultiPolygon(Seq(fixtures.extractCoordinates(subject).map(x => x: Contour)))
    val clippingPolygons = MultiPolygon(Seq(fixtures.extractCoordinates(clipping).map(x => x: Contour)))

    val bbox = Array(PositiveInfinity, PositiveInfinity, NegativeInfinity, NegativeInfinity)
    val queue = fillQueue(subjectPolygons, clippingPolygons, bbox, bbox)
    val p0 = Point(16, 282)
    val p1 = Point(298, 359)
    val p2 = Point(156, 203.5)

    val te1 = SweepEvent(p0, left = true, isSubject = true)
    te1.otherEvent = SweepEvent(p1, te1)

    val te2 = SweepEvent(p0, left = true, isSubject = true)
    te2.otherEvent = SweepEvent(p2, te2, left = true)

    val tree = new AVLTree[SweepEvent]()(SweepEvent.CompareSegments)

    tree.insert(te1)
    tree.insert(te2)

    assert(tree.findNode(te1).exists(_.key == te1))
    assert(tree.findNode(te2).exists(_.key == te2))

    assert((te1 <=> te2) == 1)
    assert((te2 <=> te1) == -1)

    val segments = subdivideSegments(queue, subjectPolygons, clippingPolygons, bbox, bbox, Operation.Intersection)
    val leftSegments = segments.filter(_.left)

    assert(leftSegments.length == 11)

    val E = Point(16, 282)
    val I = Point(100.79403384562252, 233.41363754101192)
    val G = Point(298, 359)
    val C = Point(153, 294.5)
    val J = Point(203.36313843035356, 257.5101243166895)
    val F = Point(153, 203.5)
    val D = Point(56, 181)
    val A = Point(108.5, 120)
    val B = Point(241.5, 229.5)

    val intervals = Map(
      "EI" -> Interval(E, I, inOut = false, otherInOut = true, inResult = false),
      "IF" -> Interval(I, F, inOut = false, otherInOut = false, inResult = true),
      "FJ" -> Interval(F, J, inOut = false, otherInOut = false, inResult = true),
      "JG" -> Interval(J, G, inOut = false, otherInOut = true, inResult = false),
      "EG" -> Interval(E, G, inOut = true, otherInOut = true, inResult = false),
      "DA" -> Interval(D, A, inOut = false, otherInOut = true, inResult = false),
      "AB" -> Interval(A, B, inOut = false, otherInOut = true, inResult = false),
      "JB" -> Interval(J, B, inOut = true, otherInOut = true, inResult = false),
      "CJ" -> Interval(C, J, inOut = true, otherInOut = false, inResult = true, Interval(F, J)),
      "IC" -> Interval(I, C, inOut = true, otherInOut = false, inResult = true, Interval(I, F)),
      "DI" -> Interval(D, I, inOut = true, otherInOut = true, inResult = false)
    )

    for ((key, interval) <- intervals) {
      val containsInterval = leftSegments.exists(segment => {
        segment.point == interval.left &&
          segment.otherEvent.point == interval.right &&
          segment.inOut == interval.inOut &&
          segment.otherInOut == interval.otherInOut &&
          segment.inResult == interval.inResult &&
          ((segment.prevInResult == null && interval.prevInResult == null) ||
            (segment.prevInResult.point == interval.prevInResult.left &&
              segment.prevInResult.otherEvent.point == interval.prevInResult.right))
      })

      assert(containsInterval)
    }
  }

  private case class Interval(
    left: Point,
    right: Point,
    inOut: Boolean = false,
    otherInOut: Boolean = false,
    inResult: Boolean = false,
    prevInResult: Interval = null
  )

}
