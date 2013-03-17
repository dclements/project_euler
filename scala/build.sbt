name := "Project Euler Test Code"

version := "1.1"

scalaVersion := "2.10.1"

scalacOptions ++= Seq("-optimize", "-deprecation", "-feature") 

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "oBiBa" at "http://maven.obiba.org/maven2"

libraryDependencies += "org.jscience" % "jscience" % "4.3.1"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.1.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

