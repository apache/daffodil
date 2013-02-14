name := "daffodil"

scalaVersion := "2.9.2"

libraryDependencies := Seq(
  "edu.illinois.ncsa" %% "daffodil-core" % "0.7.0",
  "net.sourceforge.saxon" % "saxon" % "9.1.0.8" classifier "" classifier "dom" classifier "jdom" classifier "s9api" classifier "xpath"
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
