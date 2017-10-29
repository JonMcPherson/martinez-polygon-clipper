name := "martinez-polygon-clipper"

version := "1.0.0"

scalaVersion := "2.11.11"

resolvers += "Artifactory Realm" at "https://deadmandungeons.com/artifactory/sbt"

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-collections4" % "4.1",
  "com.soulfiresoft" %% "avl-tree" % "1.1.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)