package com.soulfiresoft.geometry.martinez

import java.lang.Math._

import com.soulfiresoft.collections.AVLTree
import com.soulfiresoft.geometry.martinez.Operation._

import scala.Double._
import scala.collection.mutable

object PolygonClipper {

  private var contourId: Long = 0

  def intersection(subject: MultiPolygon, clipping: MultiPolygon): MultiPolygon = boolean(subject, clipping, Intersection)

  def union(subject: MultiPolygon, clipping: MultiPolygon): MultiPolygon = boolean(subject, clipping, Union)

  def diff(subject: MultiPolygon, clipping: MultiPolygon): MultiPolygon = boolean(subject, clipping, Difference)

  def xor(subject: MultiPolygon, clipping: MultiPolygon): MultiPolygon = boolean(subject, clipping, XOR)

  def boolean(subject: MultiPolygon, clipping: MultiPolygon, operation: Operation): MultiPolygon = {
    if (subject.length * clipping.length == 0) {
      return trivialEmptyOpResult(subject, clipping, operation)
    }

    val sbbox = Array(PositiveInfinity, PositiveInfinity, NegativeInfinity, NegativeInfinity)
    val cbbox = Array(PositiveInfinity, PositiveInfinity, NegativeInfinity, NegativeInfinity)

    val eventQueue = fillQueue(subject, clipping, sbbox, cbbox)

    if (isDisjointPolygons(sbbox, cbbox)) {
      return trivialDisjointOpResult(subject, clipping, sbbox, cbbox, operation)
    }
    val sortedEvents = subdivideSegments(eventQueue, subject, clipping, sbbox, cbbox, operation)
    connectEdges(sortedEvents, operation)
  }


  private def trivialEmptyOpResult(subject: MultiPolygon, clipping: MultiPolygon, operation: Operation) = {
    operation match {
      case Intersection => Nil
      case Difference => subject
      case _ => if (subject.isEmpty) clipping else subject
    }
  }

  private def trivialDisjointOpResult(subject: MultiPolygon, clipping: MultiPolygon, sbbox: Array[Double], cbbox: Array[Double], operation: Operation) = {
    operation match {
      case Intersection => Nil
      case Difference => subject
      case _ => subject ++ clipping
    }
  }

  private def isDisjointPolygons(bbox1: Array[Double], bbox2: Array[Double]) = {
    bbox1(0) > bbox2(2) || bbox2(0) > bbox1(2) || bbox1(1) > bbox2(3) || bbox2(1) > bbox1(3)
  }

  private[geometry] def fillQueue(subject: MultiPolygon, clipping: MultiPolygon, sbbox: Array[Double], cbbox: Array[Double]): mutable.PriorityQueue[SweepEvent] = {
    val eventQueue = new mutable.PriorityQueue[SweepEvent]()(Ordering[SweepEvent].reverse)

    for (multiPolygon <- subject; index <- multiPolygon.indices) {
      processPolygon(multiPolygon(index), sbbox, eventQueue, index == 0, isSubject = true)
    }

    for (multiPolygon <- clipping; index <- multiPolygon.indices) {
      processPolygon(multiPolygon(index), cbbox, eventQueue, index == 0, isSubject = false)
    }
    eventQueue
  }

  private def processPolygon(
    contourOrHole: Contour,
    bbox: Array[Double],
    eventQueue: mutable.PriorityQueue[SweepEvent],
    isExteriorRing: Boolean,
    isSubject: Boolean) {

    if (isExteriorRing) contourId += 1

    for (i <- 0 until contourOrHole.length - 1) {
      val s1 = contourOrHole(i)
      val s2 = contourOrHole(i + 1)

      val e1 = SweepEvent(s1, isSubject = isSubject)
      val e2 = SweepEvent(s2, e1, isSubject)
      e1.otherEvent = e2

      e1.contourId = contourId
      e2.contourId = contourId
      if (!isExteriorRing) {
        e1.isExteriorRing = false
        e2.isExteriorRing = false
      }
      if (e1 > e2) {
        e2.left = true
      } else {
        e1.left = true
      }

      bbox(0) = min(bbox(0), s1.x)
      bbox(1) = min(bbox(1), s1.y)
      bbox(2) = max(bbox(2), s1.x)
      bbox(3) = max(bbox(3), s1.y)

      eventQueue += e1
      eventQueue += e2
    }
  }


  private def connectEdges(sortedEvents: Seq[SweepEvent], operation: Operation) = {
    val resultEvents = orderEvents(sortedEvents)

    val processed = Array.fill(resultEvents.length)(false)
    val result = mutable.ArrayBuffer[mutable.ArrayBuffer[mutable.ArrayBuffer[Any]]]()

    for (index <- resultEvents.indices) {
      if (!processed(index)) {
        var contour = mutable.ArrayBuffer(mutable.ArrayBuffer[Any]())

        if (!resultEvents(index).isExteriorRing) {
          if (result.isEmpty) {
            result += mutable.ArrayBuffer(mutable.ArrayBuffer(contour))
          } else if (operation == Union /* || operation == XOR */ ) {
            //result.last += contour(0)
            result += contour
          } else {
            result.last += contour.asInstanceOf[mutable.ArrayBuffer[Any]]
          }
        } else {
          result += contour
        }

        val ringId = result.length - 1
        var pos = index

        var initial = resultEvents(index).point
        // initial.push(resultEvents[i].isExteriorRing);
        contour(0) += initial

        while (pos >= index) {
          val event = resultEvents(pos)
          processed(pos) = true

          if (event.left) {
            event.resultInOut = false
            event.contourId = ringId
          } else {
            event.otherEvent.resultInOut = true
            event.otherEvent.contourId = ringId
          }

          pos = event.pos
          processed(pos) = true
          // resultEvents[pos].point.push(resultEvents[pos].isExteriorRing);

          contour(0) += resultEvents(pos).point
          pos = nextPos(pos, resultEvents, processed, index)
        }

        pos = if (pos == -1) index else pos

        val event = resultEvents(pos)
        processed(pos) = true
        processed(event.pos) = true
        event.otherEvent.resultInOut = true
        event.otherEvent.contourId = ringId
      }
    }

    for (polygon <- result; contourIndex <- polygon.indices; point <- polygon(contourIndex)) {
      if (!point.isInstanceOf[Point]) {
        polygon += point.asInstanceOf[mutable.ArrayBuffer[mutable.ArrayBuffer[Any]]](0)
        polygon.remove(contourIndex)
      }
    }

    result.map(x =>
      x.map(y =>
        y.map(z =>
          z.asInstanceOf[Point]
        ): Contour
      ): Polygon
    ): MultiPolygon
  }

  private def orderEvents(sortedEvents: Seq[SweepEvent]) = {
    var resultEvents = mutable.Buffer[SweepEvent]()

    for (event <- sortedEvents) {
      if ((event.left && event.inResult) || (!event.left && event.otherEvent.inResult)) {
        resultEvents += event
      }
    }

    // Due to overlapping edges the resultEvents array can be not wholly sorted
    var sorted = false
    while (!sorted) {
      sorted = true
      for (index <- 0 until resultEvents.length - 1) {
        val nextIndex = index + 1
        if (resultEvents(index) > resultEvents(nextIndex)) {
          val tmp = resultEvents(index)
          resultEvents(index) = resultEvents(nextIndex)
          resultEvents(nextIndex) = tmp
          sorted = false
        }
      }
    }

    for (index <- resultEvents.indices) {
      resultEvents(index).pos = index
    }

    for (event <- resultEvents) {
      if (!event.left) {
        val tmp = event.pos
        event.pos = event.otherEvent.pos
        event.otherEvent.pos = tmp
      }
    }
    resultEvents
  }

  private def nextPos(pos: Int, resultEvents: Seq[SweepEvent], processed: Array[Boolean], origIndex: Int): Int = {
    val length = resultEvents.length
    val point = resultEvents(pos).point
    var newPos = pos + 1
    var newPoint = resultEvents(newPos).point

    while (newPos < length && newPoint == point) {
      if (!processed(newPos)) {
        return newPos
      } else {
        newPos += 1
        newPoint = resultEvents(newPos).point
      }
    }

    newPos = pos - 1

    while (newPos >= 0 && processed(newPos) && newPos >= origIndex) {
      newPos -= 1
    }
    newPos
  }

  // ------- CORE ALGORITHM -------

  private[geometry] def subdivideSegments(
    eventQueue: mutable.PriorityQueue[SweepEvent],
    subject: MultiPolygon,
    clipping: MultiPolygon,
    sbbox: Array[Double],
    cbbox: Array[Double],
    operation: Operation): Seq[SweepEvent] = {

    val sweepLine = new AVLTree[SweepEvent]()(SweepEvent.CompareSegments)
    val sortedEvents = mutable.ArrayBuffer[SweepEvent]()

    val rightbound = min(sbbox(2), cbbox(2))

    while (eventQueue.nonEmpty) {
      var event = eventQueue.dequeue()
      sortedEvents += event

      // optimization by bboxes for intersection and difference goes here
      if ((operation == Intersection && event.point.x > rightbound) || (operation == Difference && event.point.x > sbbox(2))) {
        return sortedEvents
      }

      var begin: sweepLine.AVLNode = null
      if (event.left) {
        val current = sweepLine.insert(event)

        begin = sweepLine.minNode()
        val prev = if (current != begin) sweepLine.prev(current) else None
        val next = sweepLine.next(current)

        val prevEvent = prev.map(_.key)
        val nextEvent = next.map(_.key)

        computeFields(event, prevEvent, operation)
        nextEvent.foreach(n => {
          if (possibleIntersection(event, n, eventQueue) == 2) {
            computeFields(event, prevEvent, operation)
            computeFields(event, nextEvent, operation)
          }
        })
        prevEvent.foreach(p => {
          if (possibleIntersection(p, event, eventQueue) == 2) {
            val prevprevEvent = if (!prev.contains(begin)) prev.flatMap(sweepLine.prev).map(_.key) else None

            computeFields(p, prevprevEvent, operation)
            computeFields(event, prevEvent, operation)
          }
        })
      } else {
        event = event.otherEvent
        val current = sweepLine.findNode(event)

        current.foreach(current => {
          val prev = if (current != begin) sweepLine.prev(current).map(_.key) else None
          val next = sweepLine.next(current).map(_.key)

          sweepLine.remove(event)

          for (prevEvent <- prev; nextEvent <- next) {
            possibleIntersection(prevEvent, nextEvent, eventQueue)
          }
        })
      }
    }
    sortedEvents
  }


  private[geometry] def possibleIntersection(event1: SweepEvent, event2: SweepEvent, eventQueue: mutable.PriorityQueue[SweepEvent]): Int = {
    // that disallows self-intersecting polygons,
    // did cost us half a day, so I'll leave it
    // out of respect
    // if (se1.isSubject === se2.isSubject) return;
    val inter = intersection(
      event1.point, event1.otherEvent.point,
      event2.point, event2.otherEvent.point
    )

    val intersections = if (inter != null) inter.length else 0
    if (intersections == 0) return 0 // no intersection

    // the line segments intersect at an endpoint of both line segments
    if ((intersections == 1) && (event1.point == event2.point || event1.otherEvent.point == event2.otherEvent.point)) {
      return 0
    }

    if (intersections == 2 && event1.isSubject == event2.isSubject) {
      // if(se1.contourId === se2.contourId){
      // console.warn('Edges of the same polygon overlap',
      //   se1.point, se1.otherEvent.point, se2.point, se2.otherEvent.point);
      // }
      //throw new Error('Edges of the same polygon overlap');
      return 0
    }

    // The line segments associated to se1 and se2 intersect
    if (intersections == 1) {

      // if the intersection point is not an endpoint of se1
      if (event1.point != inter(0) && event1.otherEvent.point != inter(0)) {
        divideSegment(event1, inter(0), eventQueue)
      }

      // if the intersection point is not an endpoint of se2
      if (event2.point != inter(0) && event2.otherEvent.point != inter(0)) {
        divideSegment(event2, inter(0), eventQueue)
      }
      return 1
    }

    // The line segments associated to se1 and se2 overlap
    val events = mutable.ArrayBuffer[SweepEvent]()
    var leftCoincide = false
    var rightCoincide = false

    if (event1.point == event2.point) {
      leftCoincide = true; // linked
    } else if (event1 > event2) {
      events += (event2, event1)
    } else {
      events += (event1, event2)
    }

    if (event1.otherEvent.point == event2.otherEvent.point) {
      rightCoincide = true
    } else if (event1.otherEvent > event2.otherEvent) {
      events += (event2.otherEvent, event1.otherEvent)
    } else {
      events += (event1.otherEvent, event2.otherEvent)
    }

    if ((leftCoincide && rightCoincide) || leftCoincide) {
      // both line segments are equal or share the left endpoint
      event1.edgeType = EdgeType.NonContributing
      event2.edgeType = if (event1.inOut == event2.inOut) EdgeType.SameTransition else EdgeType.DifferentTransition

      if (leftCoincide && !rightCoincide) {
        // honestly no idea, but changing events selection from [2, 1]
        // to [0, 1] fixes the overlapping self-intersecting polygons issue
        divideSegment(events(1).otherEvent, events(0).point, eventQueue)
      }
      return 2
    }

    // the line segments share the right endpoint
    if (rightCoincide) {
      divideSegment(events(0), events(1).point, eventQueue)
      return 3
    }

    // no line segment includes totally the other one
    if (events(0) != events(3).otherEvent) {
      divideSegment(events(0), events(1).point, eventQueue)
      divideSegment(events(1), events(2).point, eventQueue)
      return 3
    }

    // one line segment includes the other one
    divideSegment(events(0), events(1).point, eventQueue)
    divideSegment(events(3).otherEvent, events(2).point, eventQueue)

    3
  }

  /**
    * Finds the intersection (if any) between two line segments a and b, given the
    * line segments' end points a1, a2 and b1, b2.
    *
    * This algorithm is based on Schneider and Eberly.
    * http://www.cimec.org.ar/~ncalvo/Schneider_Eberly.pdf
    * Page 244.
    *
    * @param a1 point of first line
    * @param a2 point of first line
    * @param b1 b1 point of second line
    * @param b2 b2 point of second line
    * @param noEndpointTouch If the lines intersect, the point of intersection.
    * If they overlap, the two end points of the overlapping segment.
    * Otherwise, null.
    */
  private[geometry] def intersection(a1: Point, a2: Point, b1: Point, b2: Point, noEndpointTouch: Boolean = false): Array[Point] = {
    // The algorithm expects our lines in the form P + sd, where P is a point,
    // s is on the interval [0, 1], and d is a vector.
    // We are passed two points. P can be the first point of each pair. The
    // vector, then, could be thought of as the distance (in x and y components)
    // from the first point to the second point.
    // So first, let's make our vectors:
    val va = Array(a2.x - a1.x, a2.y - a1.y)
    val vb = Array(b2.x - b1.x, b2.y - b1.y)

    // We also define a function to convert back to regular point form:
    def toPoint(p: Point, s: Double, d: Array[Double]) = Point(p.x + s * d(0), p.y + s * d(1))

    // The rest is pretty much a straight port of the algorithm.
    val e = Array(b1.x - a1.x, b1.y - a1.y)
    var kross = crossProduct(va, vb)
    var sqrKross = kross * kross
    val sqrLenA = dotProduct(va, va)
    val sqrLenB = dotProduct(vb, vb)

    // Check for line intersection. This works because of the properties of the
    // cross product -- specifically, two vectors are parallel if and only if the
    // cross product is the 0 vector. The full calculation involves relative error
    // to account for possible very small line segments. See Schneider & Eberly
    // for details.
    if (sqrKross > Epsilon * sqrLenA * sqrLenB) {
      // If they're not parallel, then (because these are line segments) they
      // still might not actually intersect. This code checks that the
      // intersection point of the lines is actually on both line segments.
      val s = crossProduct(e, vb) / kross
      if (s < 0 || s > 1) {
        // not on line segment a
        return null
      }
      val t = crossProduct(e, va) / kross
      if (t < 0 || t > 1) {
        // not on line segment b
        return null
      }
      return if (noEndpointTouch) null else Array(toPoint(a1, s, va))
    }

    // If we've reached this point, then the lines are either parallel or the
    // same, but the segments could overlap partially or fully, or not at all.
    // So we need to find the overlap, if any. To do that, we can use e, which is
    // the (vector) difference between the two initial points. If this is parallel
    // with the line itself, then the two lines are the same line, and there will
    // be overlap.
    val sqrLenE = dotProduct(e, e)
    kross = crossProduct(e, va)
    sqrKross = kross * kross

    if (sqrKross > Epsilon * sqrLenA * sqrLenE) {
      // Lines are just parallel, not the same. No overlap.
      return null
    }

    val sa = dotProduct(va, e) / sqrLenA
    val sb = sa + dotProduct(va, vb) / sqrLenA
    val smin = Math.min(sa, sb)
    val smax = Math.max(sa, sb)

    // this is, essentially, the FindIntersection acting on floats from
    // Schneider & Eberly, just inlined into this function.
    if (smin <= 1 && smax >= 0) {

      // overlap on an end point
      if (smin == 1) {
        return if (noEndpointTouch) null else Array(toPoint(a1, if (smin > 0) smin else 0, va))
      }

      if (smax == 0) {
        return if (noEndpointTouch) null else Array(toPoint(a1, if (smax < 1) smax else 1, va))
      }

      if (noEndpointTouch && smin == 0 && smax == 1) return null

      // There's overlap on a segment -- two points of intersection. Return both.
      return Array(toPoint(a1, if (smin > 0) smin else 0, va), toPoint(a1, if (smax < 1) smax else 1, va))
    }
    null
  }

  private[geometry] def divideSegment(event: SweepEvent, point: Point, eventQueue: mutable.PriorityQueue[SweepEvent]) = {
    val r = new SweepEvent(point, event, event.isSubject, left = false)
    val l = new SweepEvent(point, event.otherEvent, event.isSubject, left = true)

    if (event.point == event.otherEvent.point) {
      //console.warn('what is that?', se);
    }

    r.contourId = event.contourId
    l.contourId = event.contourId

    // avoid a rounding error. The left event would be processed after the right event
    if (l > event.otherEvent) {
      event.otherEvent.left = true
      l.left = false
    }

    // avoid a rounding error. The left event would be processed after the right event
    // if (event > r) {}

    event.otherEvent.otherEvent = l
    event.otherEvent = r

    eventQueue += l
    eventQueue += r

    eventQueue
  }

  private def computeFields(event: SweepEvent, prevEvent: Option[SweepEvent], operation: Operation) {
    // compute inOut and otherInOut fields
    prevEvent match {
      case Some(prev) =>
        if (event.isSubject == prev.isSubject) {
          // previous line segment in sweepline belongs to the clipping polygon
          event.inOut = !prev.inOut
          event.otherInOut = prev.otherInOut
        } else {
          event.inOut = !prev.otherInOut
          event.otherInOut = if (prev.isVertical()) !prev.inOut else prev.inOut
        }

        // compute prevInResult field
        event.prevInResult = if (!inResult(prev, operation) || prev.isVertical()) prev.prevInResult else prev
      case None =>
        // previous line segment in sweepline belongs to the same polygon
        event.inOut = false
        event.otherInOut = true
    }

    // check if the line segment belongs to the Boolean operation
    event.inResult = inResult(event, operation)
  }


  private def inResult(event: SweepEvent, operation: Operation): Boolean = {
    event.edgeType match {
      case EdgeType.Normal => operation match {
        case Intersection => !event.otherInOut
        case Union => event.otherInOut
        case Difference => (event.isSubject && event.otherInOut) || (!event.isSubject && !event.otherInOut)
        case XOR => true
      }
      case EdgeType.SameTransition => operation == Intersection || operation == Union
      case EdgeType.DifferentTransition => operation == Difference
      case EdgeType.NonContributing => false
      case _ => false
    }
  }

}
