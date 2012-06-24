// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository 
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

// Use the Play sbt plugin for Play projects
addSbtPlugin("play" % "sbt-plugin" % "2.0.1")

// ensime
resolvers += "Scala-Tools Snapshots Repository (sonatype)" at "https://oss.sonatype.org/content/groups/scala-tools/"

addSbtPlugin("org.ensime" % "ensime-sbt-cmd" % "0.0.10")
