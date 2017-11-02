

name := "daffodil"

organization in ThisBuild := "edu.illinois.ncsa"

version in ThisBuild := "2.1.0-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.8"

// incOptions := incOptions.value.withNameHashing(true) // 2.11 experimental incremental compilation improvements (perhaps not working right?)

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-Yinline-warnings", "-Xxml:-coalescing", "-language:experimental.macros", "-Ybackend:GenBCode", "-Yopt-warnings", 
 "-Ywarn-inaccessible", "-Ywarn-unused-import", "-Ywarn-unused", "-Ywarn-infer-any", "-Ywarn-nullary-override", "-Ydead-code", "-Yopt:inline-global", "-Yinline" , "-Xfatal-warnings")

// parallelExecution in ThisBuild := false

// concurrentRestrictions in Global += Tags.limit(Tags.Test, 1)

logBuffered in ThisBuild := true

testOptions in ThisBuild += Tests.Argument(TestFrameworks.JUnit, "-v")

transitiveClassifiers := Seq("sources", "javadoc")

libraryDependencies in ThisBuild := Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "junit" % "junit" % "4.11" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "net.sf.expectit" % "expectit-core" % "0.8.1" % "test",
  "org.jdom" % "jdom2" % "2.0.6",
  "com.ibm.icu" % "icu4j" % "51.1", // new versions avail. 58.1 requires code changes
  "xerces" % "xercesImpl" % "2.10.0",
  "com.fasterxml.woodstox" % "woodstox-core" % "5.0.3",
  "com.fasterxml.jackson.core" % "jackson-core" % "2.8.8",
  "xml-resolver" % "xml-resolver" % "1.2",
  "jline" % "jline" % "2.12.1", // newer versions avail. 3.0.0-M1 requires code changes
  "org.fusesource.jansi" % "jansi" % "1.14",
  "org.rogach" %% "scallop" % "0.9.5", // new version avail. 2.0.5 requires code changes
  "commons-io" % "commons-io" % "2.5",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)

initialize := {
  val _ = initialize.value
  if (sys.props("java.specification.version") != "1.8") {
    sys.error("Java 8 is required for this project.")
  }
}

retrieveManaged := true

exportJars in ThisBuild := true

exportJars in Test in ThisBuild := false





publishMavenStyle in ThisBuild := true

publishArtifact in Test := false

pomIncludeRepository in ThisBuild := { _ => false }

scmInfo := Some(
  ScmInfo(
    browseUrl = url("https://git-wip-us.apache.org/repos/asf?p=incubator-daffodil.git"),
    connection = "scm:git:git://git.apache.org/incubator-daffodil.git"
  )
)

licenses in ThisBuild := Seq("University of Illinois/NCSA Open Source License" -> url("http://opensource.org/licenses/UoI-NCSA.php"))

homepage in ThisBuild := Some(url("https://daffodil.apache.org"))
