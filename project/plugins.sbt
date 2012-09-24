addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.2.0")


libraryDependencies ++= Seq(
  "org.jacoco" % "org.jacoco.core" % "0.5.9.201207300726" artifacts(Artifact("org.jacoco.core", "jar", "jar")),
  "org.jacoco" % "org.jacoco.report" % "0.5.9.201207300726" artifacts(Artifact("org.jacoco.report", "jar", "jar"))
)

addSbtPlugin("de.johoop" % "jacoco4sbt" % "1.2.4")


addSbtPlugin("com.typesafe.startscript" % "xsbt-start-script-plugin" % "0.5.3")
