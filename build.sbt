name := "edg-ide"

version := "0.1-SNAPSHOT"

scalaVersion := "2.13.4"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.0" % "test",

  "org.eclipse.elk" % "org.eclipse.elk.alg.layered" % "0.7.0",
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

lazy val compiler = project in file("PolymorphicBlocks/compiler")  // proto imported transitively
lazy val root = (project in file("."))
  .dependsOn(compiler)
