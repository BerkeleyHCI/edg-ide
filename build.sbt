name := "edg-ide"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.10"

libraryDependencies ++= Seq(
  "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",
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
