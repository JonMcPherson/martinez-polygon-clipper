package com.soulfiresoft.geometry.martinez

import org.scalatest.FlatSpec

class SignedAreaTest extends FlatSpec {

  it should "calculate analytical signed area" in {
    assert(signedArea(Point(0, 0), Point(0, 1), Point(1, 1)) == -1, "negative area")
    assert(signedArea(Point(0, 1), Point(0, 0), Point(1, 0)) == 1, "positive area")
    assert(signedArea(Point(0, 0), Point(1, 1), Point(2, 2)) == 0, "collinear, 0 area")

    assert(signedArea(Point(-1, 0), Point(2, 3), Point(0, 1)) == 0, "point on segment")
    assert(signedArea(Point(2, 3), Point(-1, 0), Point(0, 1)) == 0, "point on segment")
  }

}
