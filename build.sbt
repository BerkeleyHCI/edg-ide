name := "edg-ide"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.10"

libraryDependencies ++= Seq(
  "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",
)

// IntelliJ plugin docs here: https://github.com/JetBrains/sbt-idea-plugin
intellijPluginName := "edg-ide"

intellijPlugins += "com.intellij.properties".toPlugin
intellijPlugins += "PythonCore".toPlugin

intellijBuild in ThisBuild := "2020.2.3"

enablePlugins(SbtIdeaPlugin)

patchPluginXml := pluginXmlOptions { xml =>
  xml.version           = version.value
  xml.pluginDescription = "EDG IDE"
  xml.sinceBuild        = (intellijBuild in ThisBuild).value
  xml.untilBuild        = "202.*"
}
