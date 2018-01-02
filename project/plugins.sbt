addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.2.2")

// Both sbt and the sbt-native-pacakger plugin have transitive dependencies to
// different versions of plexus-utils and guava, but with different major
// version numbers. SBT interprets this major version number difference as
// having dependencies that are potentially not binary compatible, resulting in
// a warning message when starting SBT. It appears the binary incompatibilities
// (if they exist) do not affect building Daffodil, so this overrides the
// dependencies to the latest versions and removes the warning.
dependencyOverrides ++= Seq(
  "org.codehaus.plexus" % "plexus-utils" % "3.0.17",
  "com.google.guava" % "guava" % "18.0"
)

addSbtPlugin("com.typesafe.sbt" % "sbt-license-report" % "1.2.0")
