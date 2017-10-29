package com.soulfiresoft.geometry.martinez

/**
  * @param point the point involved in this sweep event
  * @param otherEvent the other edge reference
  * @param isSubject whether this event belongs to source or clipping polygon
  * @param left whether point is left endpoint
  * @param edgeType the edge contribution type
  */
class SweepEvent(
  val point: Point,
  var otherEvent: SweepEvent,
  val isSubject: Boolean,
  var left: Boolean,
  var edgeType: EdgeType = EdgeType.Normal)
  extends Ordered[SweepEvent] {

  var pos = 0
  var contourId = 0L

  /**
    * Previous event in result
    */
  var prevInResult: SweepEvent = _

  /**
    * In-out transition for the sweepline crossing polygon
    */
  var inOut = false
  var otherInOut = false

  /**
    * Whether this event belongs to a result
    */
  var inResult = false

  // connection step

  var resultInOut = false
  var isExteriorRing = true


  def isBelow(p: Point): Boolean = if (left) signedArea(point, otherEvent.point, p) > 0 else signedArea(otherEvent.point, point, p) > 0

  def isAbove(p: Point): Boolean = !this.isBelow(p)

  def isVertical(): Boolean = this.point.x == this.otherEvent.point.x


  def <=>(that: SweepEvent): Int = {
    if (this == that) return 0

    // Segments are not collinear
    if (signedArea(this.point, this.otherEvent.point, that.point) != 0 ||
      signedArea(this.point, this.otherEvent.point, that.otherEvent.point) != 0) {

      // If they share their left endpoint use the right endpoint to sort
      if (this.point == that.point) return if (this.isBelow(that.otherEvent.point)) -1 else 1

      // Different left endpoint: use the left endpoint to sort
      if (this.point.x == that.point.x) return if (this.point.y < that.point.y) -1 else 1

      // has the line segment associated to e1 been inserted
      // into S after the line segment associated to e2 ?
      if (this > that) return if (that.isAbove(this.point)) -1 else 1

      // The line segment associated to e2 has been inserted
      // into S after the line segment associated to e1
      return if (this.isBelow(that.point)) -1 else 1
    }

    if (this.isSubject == that.isSubject) { // same polygon
      if (this.point == that.point) {
        if (this.otherEvent.point == that.otherEvent.point) {
          return 0
        } else {
          return if (this.contourId > that.contourId) 1 else -1
        }
      }
    } else { // Segments are collinear, but belong to separate polygons
      return if (this.isSubject) -1 else 1
    }

    if (this > that) 1 else -1
  }

  override def compare(that: SweepEvent): Int = {
    // Different x-coordinate
    if (this.point.x > that.point.x) return 1
    if (this.point.x < that.point.x) return -1

    // Different points, but same x-coordinate
    // Event with lower y-coordinate is processed first
    if (this.point.y != that.point.y) return if (this.point.y > that.point.y) 1 else -1

    specialCases(that)
  }

  private def specialCases(that: SweepEvent): Int = {
    // Same coordinates, but one is a left endpoint and the other is
    // a right endpoint. The right endpoint is processed first
    if (this.left != that.left) return if (this.left) 1 else -1

    // Same coordinates, both events
    // are left endpoints or right endpoints.
    // not collinear
    if (signedArea(this.point, this.otherEvent.point, that.otherEvent.point) != 0) {
      // the event associate to the bottom segment is processed first
      return if (!this.isBelow(that.otherEvent.point)) 1 else -1
    }

    // uncomment this if you want to play with multipolygons
    // if (e1.isSubject === e2.isSubject) {
    //   if(equals(e1.point, e2.point) && e1.contourId === e2.contourId) {
    //     return 0;
    //   } else {
    //     return e1.contourId > e2.contourId ? 1 : -1;
    //   }
    // }

    if (!this.isSubject && that.isSubject) 1 else -1
  }

  override def toString: String = s"SweepEvent($point, ${otherEvent.point})"

}

object SweepEvent {

  val CompareSegments = new Ordering[SweepEvent] {
    override def compare(x: SweepEvent, y: SweepEvent): Int = x <=> y
  }

  def apply(point: Point, otherEvent: SweepEvent = null, isSubject: Boolean = false, left: Boolean = false, edgeType: EdgeType = EdgeType.Normal): SweepEvent = {
    new SweepEvent(point, otherEvent, isSubject, left, edgeType)
  }

}
