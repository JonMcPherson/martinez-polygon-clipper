package com.soulfiresoft.geometry.martinez

import scala.io.Source
import scala.util.parsing.json.JSON

package object fixtures {

  def loadGeoJson(file: String): Map[String, Any] = {
    JSON.parseFull(Source.fromURL(getClass.getResource(file)).mkString).get.asInstanceOf[Map[String, Any]]
  }

  def loadGeoJsonSubjectClipping(file: String): (Map[String, Any], Map[String, Any]) = {
    val geoJson = loadGeoJson(file)
    (geoJson("features").asInstanceOf[List[Map[String, Any]]](0), geoJson("features").asInstanceOf[List[Map[String, Any]]](1))
  }

  def extractCoordinates(features: Map[String, Any]): List[List[Point]] = {
    features("geometry").asInstanceOf[Map[String, Any]]("coordinates").asInstanceOf[List[List[List[Double]]]]
      .map(_.map(p => Point(p(0), p(1))))
  }

}
