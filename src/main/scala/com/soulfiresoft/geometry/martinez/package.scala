package com.soulfiresoft.geometry

package object martinez {

  var Epsilon = 1E-9

  def signedArea(p0: Point, p1: Point, p2: Point): Double = {
    (p0.x - p2.x) * (p1.y - p2.y) - (p1.x - p2.x) * (p0.y - p2.y)
  }


  /**
    * Finds the magnitude of the cross product of two vectors (if we pretend they're in three dimensions)
    *
    * @param a the first vector
    * @param b the second vector
    * @return the magnitude of the cross product
    */
  def crossProduct(a: Array[Double], b: Array[Double]): Double = a(0) * b(1) - a(1) * b(0)

  /**
    * Finds the dot product of two vectors.
    *
    * @param a the first vector
    * @param b the second vector
    * @return the dot product
    */
  def dotProduct(a: Array[Double], b: Array[Double]): Double = a(0) * b(0) + a(1) * b(1)


  implicit class MultiPolygon(val polygons: Seq[Polygon]) extends Seq[Polygon] {
    override def length: Int = polygons.length
    override def apply(idx: Int): Polygon = polygons.apply(idx)
    override def iterator: Iterator[Polygon] = polygons.iterator
  }

  implicit class Polygon(val contours: Seq[Contour]) extends Seq[Contour] {
    override def length: Int = contours.length
    override def apply(idx: Int): Contour = contours.apply(idx)
    override def iterator: Iterator[Contour] = contours.iterator
  }

  implicit class Contour(val points: Seq[Point]) extends Seq[Point] {
    override def length: Int = points.length
    override def apply(idx: Int): Point = points.apply(idx)
    override def iterator: Iterator[Point] = points.iterator
  }

  case class Point(x: Double, y: Double)

}
