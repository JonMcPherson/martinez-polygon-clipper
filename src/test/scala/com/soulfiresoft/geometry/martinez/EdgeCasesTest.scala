package com.soulfiresoft.geometry.martinez

import com.soulfiresoft.geometry.martinez.PolygonClipper._
import com.soulfiresoft.geometry.martinez.{Point => P}
import org.scalatest.FlatSpec

class EdgeCasesTest extends FlatSpec {

  {
    val (subject, clipping) = fixtures.loadGeoJsonSubjectClipping("/fixtures/hourglasses.json")
    val subjectPolygons = MultiPolygon(Seq(fixtures.extractCoordinates(subject).map(y => y: Contour)))
    val clippingPolygons = MultiPolygon(Seq(fixtures.extractCoordinates(clipping).map(y => y: Contour)))

    "Touching hourglasses" should "calculate intersection correctly" in {
      val result = intersection(subjectPolygons, clippingPolygons)

      assert(result == Seq(
        Seq(Seq(P(0, 0.5), P(0.25, 0.75), P(0, 1), P(0, 0.5))),
        Seq(Seq(P(0.75, 0.75), P(1, 0.5), P(1, 1), P(0.75, 0.75)))
      ))
    }

    it should "calculate union correctly" in {
      val result = union(subjectPolygons, clippingPolygons)

      assert(result == Seq(
        Seq(Seq(P(0, 0), P(0.5, 0.5), P(0.25, 0.75), P(0.5, 1), P(0, 1.5), P(0, 1), P(0, 0.5), P(0, 0))),
        Seq(Seq(P(0.5, 0.5), P(1, 0), P(1, 0.5), P(1, 1), P(1, 1.5), P(0.5, 1), P(0.75, 0.75), P(0.5, 0.5)))
      ))
    }

    it should "calculate difference correctly" in {
      val result = diff(subjectPolygons, clippingPolygons)

      assert(result == Seq(
        Seq(Seq(P(0, 0), P(0.5, 0.5), P(0.25, 0.75), P(0, 0.5), P(0, 0))),
        Seq(Seq(P(0.5, 0.5), P(1, 0), P(1, 0.5), P(0.75, 0.75), P(0.5, 0.5)))
      ))
    }
  }

  {
    val (subject, clipping) = fixtures.loadGeoJsonSubjectClipping("/fixtures/polygon_trapezoid_edge_overlap.json")
    val subjectPolygons = MultiPolygon(Seq(fixtures.extractCoordinates(subject).map(y => y: Contour)))
    val clippingPolygons = MultiPolygon(Seq(fixtures.extractCoordinates(clipping).map(y => y: Contour)))

    "Polygon + trapezoid" should "calculate intersection correctly" in {
      val result = intersection(subjectPolygons, clippingPolygons)

      assert(result == Seq(
        Seq(Seq(P(3.5, 3.5), P(7, 0), P(14, 0), P(17.5, 3.5), P(3.5, 3.5)))
      ))
    }

    it should "calculate union correctly" in {
      val result = union(subjectPolygons, clippingPolygons)

      assert(result == Seq(
        Seq(Seq(P(0, 0), P(7, 0), P(14, 0), P(21, 0), P(21, 3.5), P(17.5, 3.5), P(21, 7), P(0, 7), P(3.5, 3.5), P(0, 3.5), P(0, 0)))
      ))
    }

    it should "calculate difference correctly" in {
      val result = diff(subjectPolygons, clippingPolygons)

      assert(result == Seq(
        Seq(Seq(P(0, 0), P(7, 0), P(3.5, 3.5), P(0, 3.5), P(0, 0))),
        Seq(Seq(P(14, 0), P(21, 0), P(21, 3.5), P(17.5, 3.5), P(14, 0)))
      ))
    }
  }

  {
    val (subject, clipping) = fixtures.loadGeoJsonSubjectClipping("/fixtures/overlap_loop.json")
    val subjectPolygons = MultiPolygon(Seq(fixtures.extractCoordinates(subject).map(y => y: Contour)))
    val clippingPolygons = MultiPolygon(Seq(fixtures.extractCoordinates(clipping).map(y => y: Contour)))

    "Overlapping edge + one inside" should "calculate intersection correctly" in {
      val result = intersection(subjectPolygons, clippingPolygons)

      assert(result == Seq(
        Seq(Seq(P(57.8, -49.1), P(177.8, -49.1), P(177.8, -37.1), P(57.8, -37.1), P(57.8, -49.1)))
      ))
    }

    it should "calculate union correctly" in {
      val result = union(subjectPolygons, clippingPolygons)

      assert(result == Seq(
        Seq(Seq(P(57.8, -97.1), P(196.4, -97.1), P(196.4, -11.5), P(57.8, -11.5), P(57.8, -37.1), P(57.8, -49.1), P(57.8, -97.1)))
      ))
    }

    it should "calculate difference correctly" in {
      val result = diff(subjectPolygons, clippingPolygons)

      assert(result == Seq())
    }
  }

  {
    val (subject, clipping) = fixtures.loadGeoJsonSubjectClipping("/fixtures/overlap_y.json")
    val subjectPolygons = MultiPolygon(Seq(fixtures.extractCoordinates(subject).map(y => y: Contour)))
    val clippingPolygons = MultiPolygon(Seq(fixtures.extractCoordinates(clipping).map(y => y: Contour)))

    "Overlapping Y shift" should "calculate intersection correctly" in {
      val result = intersection(subjectPolygons, clippingPolygons)

      assert(result == Seq(
        Seq(Seq(P(-1883, -8.5), P(-1783, -8.5), P(-1783, -3), P(-1783, -2.999999999999999), P(-1883, -3), P(-1883, -8.5)))
      ))
    }

    it should "calculate union correctly" in {
      val result = union(subjectPolygons, clippingPolygons)

      assert(result == Seq(
        Seq(Seq(P(-1883, -25), P(-1783, -25), P(-1783, -8.5), P(-1783, -3), P(-1783, -2.999999999999999), P(-1783, 75), P(-1883, 75), P(-1883, -3), P(-1883, -8.5), P(-1883, -25)))
      ))
    }

    it should "calculate difference correctly" in {
      val result = diff(subjectPolygons, clippingPolygons)

      assert(result == Seq())
    }
  }

  {
    val (subject, clipping) = fixtures.loadGeoJsonSubjectClipping("/fixtures/touching_boxes.json")
    val subjectPolygons = MultiPolygon(Seq(fixtures.extractCoordinates(subject).map(y => y: Contour)))
    val clippingPolygons = MultiPolygon(Seq(fixtures.extractCoordinates(clipping).map(y => y: Contour)))

    "touching boxes" should "calculate intersection correctly" in {
      val result = intersection(subjectPolygons, clippingPolygons)

      assert(result == Seq())
    }

    it should "calculate union correctly" in {
      val result = union(subjectPolygons, clippingPolygons)

      assert(result == Seq(
        Seq(Seq(P(0, 0), P(3, 0), P(3, 1), P(4, 1), P(4, 2), P(3, 2), P(3, 3), P(0, 3), P(0, 0)))
      ))
    }

    it should "calculate difference correctly" in {
      val result = diff(subjectPolygons, clippingPolygons)

      assert(result == Seq(
        Seq(Seq(P(0, 0), P(3, 0), P(3, 1), P(3, 2), P(3, 3), P(0, 3), P(0, 0)))
      ))
    }
  }

}
