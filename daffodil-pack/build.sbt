name := "daffodil"

scalaVersion := "2.9.2"

//version := "XXX_VERSION_XXX"

libraryDependencies ++= Seq(
  //"edu.illinois.ncsa" %% "daffodil-cli" % "XXX_VERSION_XXX",
)

// Add NCSA servers as a repository
resolvers ++= Seq(
  "NCSA Sonatype Releases" at "https://opensource.ncsa.illinois.edu/nexus/content/repositories/releases",
  "NCSA Sonatype Snapshots" at "https://opensource.ncsa.illinois.edu/nexus/content/repositories/snapshots"
)

