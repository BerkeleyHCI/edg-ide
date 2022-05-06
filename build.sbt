import org.jetbrains.sbtidea.Keys.intellijPlugins
import sbt.Keys.name
import sbtbuildinfo.BuildInfoPlugin.autoImport.buildInfoOptions

// IntelliJ plugin docs here: https://github.com/JetBrains/sbt-idea-plugin
ThisBuild / intellijPluginName := "edg-ide"
ThisBuild / intellijBuild := "2022.1"
ThisBuild / intellijPlatform := IntelliJPlatform.IdeaCommunity

lazy val compiler = (project in file("PolymorphicBlocks/compiler"))  // proto imported transitively
    .enablePlugins(SbtIdeaPlugin)  // sbt-idea-plugin doesn't import properly if this isn't enabled

lazy val root = (project in file("."))
    .dependsOn(compiler % "compile->compile; test->test")
    .enablePlugins(SbtIdeaPlugin)
    .settings(
      scalaVersion := "2.13.4",
      scalacOptions += "-deprecation",
      javacOptions ++= "--release" :: "11" :: Nil,

      name := "edg-ide",
      version := "0.1-SNAPSHOT",

      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "3.2.0" % "test",
        "org.eclipse.elk" % "org.eclipse.elk.alg.layered" % "0.7.0",
      ),
      intellijPlugins := Seq(
        "com.intellij.properties",
        "PythonCore",
      ).map(_.toPlugin),
      patchPluginXml := pluginXmlOptions { xml =>
        xml.version           = version.value
        xml.pluginDescription = "EDG IDE Description"
        xml.sinceBuild        = (ThisBuild / intellijBuild).value
        xml.untilBuild        = "221.*"
      },
    ).enablePlugins(BuildInfoPlugin)
    .settings(
      buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
      buildInfoOptions += BuildInfoOption.BuildTime,
      buildInfoPackage := "edg_ide.build",
    )
