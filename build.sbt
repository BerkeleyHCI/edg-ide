name := "edg-ide"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.10"

libraryDependencies ++= Seq(
  "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",
)

intellijPlugins += "com.intellij.properties".toPlugin
intellijPlugins += "PythonCore".toPlugin

enablePlugins(SbtIdeaPlugin)
