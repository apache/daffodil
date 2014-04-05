addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.6.0")


libraryDependencies ++= Seq(
  "org.jacoco" % "org.jacoco.core" % "0.5.9.201207300726" artifacts(Artifact("org.jacoco.core", "jar", "jar")),
  "org.jacoco" % "org.jacoco.report" % "0.5.9.201207300726" artifacts(Artifact("org.jacoco.report", "jar", "jar"))
)

addSbtPlugin("de.johoop" % "jacoco4sbt" % "2.1.5")


addSbtPlugin("com.typesafe.sbt" % "sbt-start-script" % "0.10.0")


addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.3")


addSbtPlugin("com.dadrox" % "sbt-test-reports" % "0.1")
