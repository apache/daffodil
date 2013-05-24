name := "daffodil"

scalaVersion := "2.9.2"

//version := "XXX_VERSION_XXX"

libraryDependencies ++= Seq(
  //"edu.illinois.ncsa" %% "daffodil-core" % "XXX_VERSION_XXX",
  "net.sourceforge.saxon" % "saxon" % "9.1.0.8" classifier "" classifier "dom" classifier "jdom" classifier "s9api" classifier "xpath"
)

// Add NCSA servers as a repository
resolvers ++= Seq(
  "NCSA Sonatype Releases" at "https://opensource.ncsa.illinois.edu/nexus/content/repositories/releases",
  "NCSA Sonatype Snapshots" at "https://opensource.ncsa.illinois.edu/nexus/content/repositories/snapshots"
)

