name := "daffodil"

organization in ThisBuild := "edu.illinois.ncsa"

scalaVersion in ThisBuild := "2.10.4"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-Yinline-warnings", "-Xfatal-warnings")

parallelExecution in ThisBuild := false

concurrentRestrictions in Global += Tags.limit(Tags.Test, 1)

logBuffered in ThisBuild := false

testOptions in ThisBuild += Tests.Argument(TestFrameworks.JUnit, "-v")

transitiveClassifiers := Seq("sources", "javadoc")

resolvers in ThisBuild += "NCSA Sonatype Releases" at "https://opensource.ncsa.illinois.edu/nexus/content/repositories/releases"

libraryDependencies in ThisBuild := Seq(
  "junit" % "junit" % "4.11" % "test",
  "com.novocode" % "junit-interface" % "0.10" % "test",
  "net.sf.expectit" % "expectit-core" % "0.6.1" % "test",
  "org.jdom" % "jdom2" % "2.0.5",
  "com.ibm.icu" % "icu4j" % "51.1",// classifier "" classifier "charset" classifier "localespi",
  "xerces" % "xercesImpl" % "2.10.0",
  "xml-resolver" % "xml-resolver" % "1.2",
  "jline" % "jline" % "2.12",
  "org.fusesource.jansi" % "jansi" % "1.11",
  "org.rogach" %% "scallop" % "0.9.5",
  "commons-io" % "commons-io" % "2.4"
)

retrieveManaged := true

exportJars in ThisBuild := true

exportJars in Test in ThisBuild := false





publishMavenStyle in ThisBuild := true

publishArtifact in Test := false

publishTo in ThisBuild := {
  val nexus = "https://opensource.ncsa.illinois.edu/nexus/"
  if (isSnapshot.value)
    Some("NCSA Sonatype Nexus Snapshot" at nexus + "content/repositories/snapshots")
  else
    Some("NCSA Sonatype Nexus Release" at nexus + "content/repositories/releases")
}

pomIncludeRepository in ThisBuild := { _ => false }

pomExtra in ThisBuild := (
  <developers>
    <developer>
      <id>Tresys Technology</id>
      <name>Tresys Technology</name>
      <url>http://www.tresys.com</url>
    </developer>
  </developers>
)

scmInfo := Some(
  ScmInfo(
    browseUrl = url("https://opensource.ncsa.illinois.edu/stash/projects/DFDL/repos/daffodil/browse"),
    connection = "scm:git:https://opensource.ncsa.illinois.edu/stash/scm/dfdl/daffodil.git"
  )
)

licenses in ThisBuild := Seq("University of Illinois/NCSA Open Source License" -> url("http://opensource.org/licenses/UoI-NCSA.php"))

homepage in ThisBuild := Some(url("https://opensource.ncsa.illinois.edu/confluence/display/DFDL/Home"))
