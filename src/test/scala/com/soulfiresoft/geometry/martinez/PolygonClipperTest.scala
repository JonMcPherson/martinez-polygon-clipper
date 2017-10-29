package com.soulfiresoft.geometry.martinez

import com.soulfiresoft.geometry.martinez.PolygonClipper._
import org.scalatest.FlatSpec

import scala.Double._

class PolygonClipperTest extends FlatSpec {

  private val (subject, clipping) = fixtures.loadGeoJsonSubjectClipping("/fixtures/two_triangles.json")
  private val subjectPolygons = MultiPolygon(Seq(fixtures.extractCoordinates(subject).map(x => x: Contour)))
  private val clippingPolygons = MultiPolygon(Seq(fixtures.extractCoordinates(clipping).map(x => x: Contour)))


  it should "correctly fill event queue" in {
    val sbbox = Array(PositiveInfinity, PositiveInfinity, NegativeInfinity, NegativeInfinity)
    val cbbox = Array(PositiveInfinity, PositiveInfinity, NegativeInfinity, NegativeInfinity)

    val queue = fillQueue(subjectPolygons, clippingPolygons, sbbox, cbbox)

    assert(queue.length == 12)

    assert(sbbox.sameElements(Seq(20, -113.5, 226.5, 74)), "subject bbox")
    assert(cbbox.sameElements(Seq(54.5, -198, 239.5, 33.5)), "clipping bbox")

    val expectations = Seq(
      SweepEvent(Point(20, -23.5), SweepEvent(Point(226.5, -113.5)), left = true),
      SweepEvent(Point(20, -23.5), SweepEvent(Point(170, 74)), left = true),
      SweepEvent(Point(54.5, -170.5), SweepEvent(Point(239.5, -198)), left = true),
      SweepEvent(Point(54.5, -170.5), SweepEvent(Point(140.5, 33.5)), left = true),
      SweepEvent(Point(140.5, 33.5), SweepEvent(Point(54.5, -170.5), left = true)),
      SweepEvent(Point(140.5, 33.5), SweepEvent(Point(239.5, -198)), left = true),
      SweepEvent(Point(170, 74), SweepEvent(Point(20, -23.5), left = true)),
      SweepEvent(Point(170, 74), SweepEvent(Point(226.5, -113.5)), left = true),
      SweepEvent(Point(226.5, -113.5), SweepEvent(Point(20, -23.5), left = true)),
      SweepEvent(Point(226.5, -113.5), SweepEvent(Point(170, 74), left = true)),
      SweepEvent(Point(239.5, -198), SweepEvent(Point(54.5, -170.5), left = true)),
      SweepEvent(Point(239.5, -198), SweepEvent(Point(140.5, 33.5), left = true))
    )

    expectations.foreach(expected => {
      val actual = queue.dequeue()

      assert(actual.point == expected.point)
      assert(actual.left == expected.left)
      assert(actual.otherEvent.point == expected.otherEvent.point)
      assert(actual.otherEvent.left == expected.otherEvent.left)
    })
  }

}
