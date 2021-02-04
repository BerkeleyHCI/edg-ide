import scalapb.compiler.Version.{grpcJavaVersion, scalapbVersion}

name := "edg-ide"

version := "0.1-SNAPSHOT"

scalaVersion := "2.13.4"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.0" % "test",

  "com.thesamet.scalapb" %% "scalapb-runtime" % scalapbVersion % "protobuf",
  "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapbVersion,
  "io.grpc" % "grpc-netty" % grpcJavaVersion,

  "org.eclipse.elk" % "org.eclipse.elk.alg.layered" % "0.7.0",
  "org.eclipse.elk" % "org.eclipse.elk.graph.json" % "0.7.0",
)

PB.protoSources in Compile := Seq(
  baseDirectory.value / "PolymorphicBlocks/edgir",
)

PB.targets in Compile := Seq(
  scalapb.gen() -> (sourceManaged in Compile).value / "scalapb"
)

// IntelliJ plugin docs here: https://github.com/JetBrains/sbt-idea-plugin
intellijPluginName in ThisBuild := "edg-ide"
intellijBuild in ThisBuild := "2020.2.3"

intellijPlugins += "com.intellij.properties".toPlugin
intellijPlugins += "PythonCore:202.7660.27".toPlugin

enablePlugins(SbtIdeaPlugin)

patchPluginXml := pluginXmlOptions { xml =>
  xml.version           = version.value
  xml.pluginDescription = "EDG IDE Description"
  xml.sinceBuild        = (intellijBuild in ThisBuild).value
  xml.untilBuild        = "202.*"
}

lazy val compiler = project in file("PolymorphicBlocks/compiler")
lazy val root = (project in file("."))
  .dependsOn(compiler)
