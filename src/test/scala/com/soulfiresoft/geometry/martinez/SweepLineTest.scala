package com.soulfiresoft.geometry.martinez

import com.soulfiresoft.collections.AVLTree
import org.scalatest.FlatSpec

class SweepLineTest extends FlatSpec {

  private val (subject, clipping) = fixtures.loadGeoJsonSubjectClipping("/fixtures/two_triangles.json")

  it should "match" in {
    val subjectPoints = fixtures.extractCoordinates(subject)
    val clippingPoints = fixtures.extractCoordinates(clipping)

    val ef = SweepEvent(subjectPoints(0)(0), SweepEvent(subjectPoints(0)(2)), left = true, isSubject = true)
    val eg = SweepEvent(subjectPoints(0)(0), SweepEvent(subjectPoints(0)(1)), left = true, isSubject = true)

    val tree = new AVLTree[SweepEvent]()(SweepEvent.CompareSegments)
    tree.insert(ef)
    tree.insert(eg)

    assert(tree.findNode(ef).isDefined, "able to retrieve node")
    assert(tree.minNode().key == ef, "EF is at the head")
    assert(tree.maxNode().key == eg, "EG is at the tail")

    val n1 = tree.findNode(ef).get
    assert(tree.next(n1).get.key == eg)

    val n2 = tree.findNode(eg).get
    assert(tree.prev(n2).get.key == ef)

    val da = SweepEvent(clippingPoints(0)(0), SweepEvent(clippingPoints(0)(2)), left = true, isSubject = true)
    val dc = SweepEvent(clippingPoints(0)(0), SweepEvent(clippingPoints(0)(1)), left = true, isSubject = true)

    tree.insert(da)
    tree.insert(dc)

    var begin = tree.minNode()

    assert(begin.key == da, "DA")
    begin = tree.next(begin).get
    assert(begin.key == dc, "DC")
    begin = tree.next(begin).get
    assert(begin.key == ef, "EF")
    begin = tree.next(begin).get
    assert(begin.key == eg, "EG")
  }

}
