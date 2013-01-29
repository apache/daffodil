name := "daffodil"

scalaVersion := "2.9.2"

libraryDependencies := Seq(
  "daffodil-core" %% "daffodil-core" % "0.6.0"
)

// Add NCSA servers as a repository
resolvers ++= Seq(
  "NCSA Sonatype Releases" at "https://opensource.ncsa.illinois.edu/nexus/content/repositories/releases",
  "NCSA Sonatype Snapshots" at "https://opensource.ncsa.illinois.edu/nexus/content/repositories/snapshots"
)

// Get the version of daffodil-core
version <<= libraryDependencies(
  _.find( _.name == "daffodil-core")
  .getOrElse(sys.error("daffodil-core not specified as a dependency"))
  .revision
)
