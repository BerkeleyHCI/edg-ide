logLevel := Level.Warn

addSbtPlugin("org.jetbrains" % "sbt-idea-plugin" % "3.7.7")

addSbtPlugin("com.thesamet" % "sbt-protoc" % "0.99.34")

libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.0-M3"
