package com.soulfiresoft.geometry.martinez

import com.soulfiresoft.geometry.martinez.PolygonClipper.intersection
import com.soulfiresoft.geometry.martinez.{Point => P}
import org.scalatest.FlatSpec

class SegmentIntersectionTest extends FlatSpec {

  it should "calculate intersection between segments" in {
    assert(intersection(P(0, 0), P(1, 1), P(1, 0), P(2, 2)) == null, "null if no intersections")
    assert(intersection(P(0, 0), P(1, 1), P(1, 0), P(10, 2)) == null, "null if no intersections")

    assert(intersection(P(0, 0), P(1, 1), P(1, 0), P(0, 1)) sameElements Array(P(0.5, 0.5)), "1 intersection")
    assert(intersection(P(0, 0), P(1, 1), P(0, 1), P(0, 0)) sameElements Array(P(0, 0)), "shared point 1")
    assert(intersection(P(0, 0), P(1, 1), P(0, 1), P(1, 1)) sameElements Array(P(1, 1)), "shared point 2")

    assert(intersection(P(0, 0), P(1, 1), P(0.5, 0.5), P(1, 0)) sameElements Array(P(0.5, 0.5)), "T-crossing")

    assert(intersection(P(0, 0), P(10, 10), P(1, 1), P(5, 5)) sameElements Array(P(1, 1), P(5, 5)), "full overlap")
    assert(intersection(P(1, 1), P(10, 10), P(1, 1), P(5, 5)) sameElements Array(P(1, 1), P(5, 5)), "shared point + overlap")
    assert(intersection(P(3, 3), P(10, 10), P(0, 0), P(5, 5)) sameElements Array(P(3, 3), P(5, 5)), "mutual overlap")
    assert(intersection(P(0, 0), P(1, 1), P(0, 0), P(1, 1)) sameElements Array(P(0, 0), P(1, 1)), "full overlap")
    assert(intersection(P(1, 1), P(0, 0), P(0, 0), P(1, 1)) sameElements Array(P(1, 1), P(0, 0)), "full overlap, orientation")

    assert(intersection(P(0, 0), P(1, 1), P(1, 1), P(2, 2)) sameElements Array(P(1, 1)), "collinear, shared point")
    assert(intersection(P(1, 1), P(0, 0), P(1, 1), P(2, 2)) sameElements Array(P(1, 1)), "collinear, shared other point")
    assert(intersection(P(0, 0), P(1, 1), P(2, 2), P(4, 4)) == null, "collinear, no overlap")
    assert(intersection(P(0, 0), P(1, 1), P(0, -1), P(1, 0)) == null, "parallel")
    assert(intersection(P(1, 1), P(0, 0), P(0, -1), P(1, 0)) == null, "parallel, orientation")
    assert(intersection(P(0, -1), P(1, 0), P(0, 0), P(1, 1)) == null, "parallel, position")

    assert(intersection(P(0, 0), P(1, 1), P(0, 1), P(0, 0), noEndpointTouch = true) == null, "shared point 1, skip touches")
    assert(intersection(P(0, 0), P(1, 1), P(0, 1), P(1, 1), noEndpointTouch = true) == null, "shared point 2, skip touches")

    assert(intersection(P(0, 0), P(1, 1), P(1, 1), P(2, 2), noEndpointTouch = true) == null, "collinear, shared point, skip touches")
    assert(intersection(P(1, 1), P(0, 0), P(1, 1), P(2, 2), noEndpointTouch = true) == null, "collinear, shared other point, skip touches")

    assert(intersection(P(0, 0), P(1, 1), P(0, 0), P(1, 1), noEndpointTouch = true) == null, "full overlap, skip touches")
    assert(intersection(P(1, 1), P(0, 0), P(0, 0), P(1, 1), noEndpointTouch = true) == null, "full overlap, orientation, skip touches")
  }

}
