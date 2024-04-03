/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import scala.collection.immutable.ListSet

import sbtcc._

lazy val genManaged = taskKey[Unit]("Generate managed sources and resources")
lazy val genProps = taskKey[Seq[File]]("Generate properties scala source")
lazy val genSchemas = taskKey[Seq[File]]("Generate DFDL schemas")
lazy val genCExamples = taskKey[Seq[File]]("Generate C example files")
lazy val genVersion = taskKey[Seq[File]]("Generate VERSION file")

lazy val daffodil = project
  .in(file("."))
  .enablePlugins(JavaUnidocPlugin, ScalaUnidocPlugin)
  .aggregate(
    cli,
    codeGenC,
    core,
    io,
    japi,
    lib,
    macroLib,
    propgen,
    runtime1,
    runtime1Layers,
    runtime1Unparser,
    sapi,
    schematron,
    slf4jLogger,
    tdmlLib,
    tdmlProc,
    testDaf,
    testIBM1,
    // testIntegration, // integration tests must be run manually
    testStdLayout,
    tutorials,
    udf,
  )
  .settings(commonSettings, nopublish, ratSettings, unidocSettings, genCExamplesSettings)

lazy val macroLib = Project("daffodil-macro-lib", file("daffodil-macro-lib"))
  .settings(commonSettings, nopublish)
  .settings(libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value)
  .disablePlugins(OsgiCheckPlugin)

lazy val propgen = Project("daffodil-propgen", file("daffodil-propgen"))
  .settings(commonSettings, nopublish)

lazy val slf4jLogger = Project("daffodil-slf4j-logger", file("daffodil-slf4j-logger"))
  .settings(commonSettings)
  .settings(libraryDependencies ++= Dependencies.slf4jAPI)

lazy val lib = Project("daffodil-lib", file("daffodil-lib"))
  .dependsOn(macroLib % "compile-internal, test-internal", slf4jLogger % "test")
  .settings(commonSettings, libManagedSettings, usesMacros)

lazy val io = Project("daffodil-io", file("daffodil-io"))
  .dependsOn(lib, macroLib % "compile-internal, test-internal", slf4jLogger % "test")
  .settings(commonSettings, usesMacros)

lazy val runtime1 = Project("daffodil-runtime1", file("daffodil-runtime1"))
  .dependsOn(
    io,
    lib % "test->test",
    udf,
    macroLib % "compile-internal, test-internal",
    slf4jLogger % "test",
  )
  .settings(commonSettings, usesMacros)

lazy val runtime1Unparser =
  Project("daffodil-runtime1-unparser", file("daffodil-runtime1-unparser"))
    .dependsOn(
      runtime1,
      lib % "test->test",
      runtime1 % "test->test",
      runtime1Layers,
      slf4jLogger % "test",
    )
    .settings(commonSettings)

lazy val runtime1Layers = Project("daffodil-runtime1-layers", file("daffodil-runtime1-layers"))
  .dependsOn(runtime1, lib % "test->test", slf4jLogger % "test")
  .settings(commonSettings)

val codeGenCLib = Library("libruntime.a")
lazy val codeGenC = Project("daffodil-codegen-c", file("daffodil-codegen-c"))
  .enablePlugins(CcPlugin)
  .dependsOn(core, core % "test->test", slf4jLogger % "test")
  .settings(commonSettings)
  .settings(
    Compile / cCompiler := sys.env.getOrElse("CC", "cc"),
    Compile / ccArchiveCommand := sys.env.getOrElse("AR", "ar"),
    Compile / ccTargets := ListSet(codeGenCLib),
    Compile / cSources := Map(
      codeGenCLib -> ((Compile / resourceDirectory).value / "org" / "apache" / "daffodil" / "codegen" / "c" / "files"
        * GlobFilter("lib*") * GlobFilter("*.c")).get(),
    ),
    Compile / cIncludeDirectories := Map(
      codeGenCLib -> Seq(
        (Compile / resourceDirectory).value / "org" / "apache" / "daffodil" / "codegen" / "c" / "files" / "libcli",
        (Compile / resourceDirectory).value / "org" / "apache" / "daffodil" / "codegen" / "c" / "files" / "libruntime",
      ),
    ),
    Compile / cFlags := (Compile / cFlags).value
      .withDefaultValue(Seq("-Wall", "-Wextra", "-Wpedantic", "-std=gnu11")),
  )

lazy val core = Project("daffodil-core", file("daffodil-core"))
  .dependsOn(
    runtime1Unparser,
    udf,
    lib % "test->test",
    runtime1 % "test->test",
    io % "test->test",
    slf4jLogger % "test",
  )
  .settings(commonSettings)

lazy val japi = Project("daffodil-japi", file("daffodil-japi"))
  .dependsOn(core, slf4jLogger % "test")
  .settings(commonSettings)

lazy val sapi = Project("daffodil-sapi", file("daffodil-sapi"))
  .dependsOn(core, slf4jLogger % "test")
  .settings(commonSettings)

lazy val tdmlLib = Project("daffodil-tdml-lib", file("daffodil-tdml-lib"))
  .dependsOn(macroLib % "compile-internal", lib, io, io % "test->test", slf4jLogger % "test")
  .settings(commonSettings)

lazy val tdmlProc = Project("daffodil-tdml-processor", file("daffodil-tdml-processor"))
  .dependsOn(tdmlLib, codeGenC, core, slf4jLogger)
  .settings(commonSettings)

lazy val cli = Project("daffodil-cli", file("daffodil-cli"))
  .dependsOn(
    tdmlProc,
    codeGenC,
    sapi,
    japi,
    schematron % Runtime,
    slf4jLogger,
  ) // causes codegen-c/sapi/japi to be pulled into the helper zip/tar
  .settings(commonSettings, nopublish)
  .settings(libraryDependencies ++= Dependencies.cli)
  .settings(libraryDependencies ++= Dependencies.exi)

lazy val udf = Project("daffodil-udf", file("daffodil-udf"))
  .dependsOn(slf4jLogger % "test")
  .settings(commonSettings)

lazy val schematron = Project("daffodil-schematron", file("daffodil-schematron"))
  .dependsOn(lib, sapi % Test, slf4jLogger % "test")
  .settings(commonSettings)
  .settings(libraryDependencies ++= Dependencies.schematron)

lazy val testDaf = Project("daffodil-test", file("daffodil-test"))
  .dependsOn(tdmlProc % "test", codeGenC % "test->test", udf % "test->test")
  .settings(commonSettings, nopublish)
//
// Uncomment the following line to run these tests
// against IBM DFDL using the Cross Tester
//
//.settings(IBMDFDLCrossTesterPlugin.settings)

lazy val testIBM1 = Project("daffodil-test-ibm1", file("daffodil-test-ibm1"))
  .dependsOn(tdmlProc % "test")
  .settings(commonSettings, nopublish)
//
// Uncomment the following line to run these tests
// against IBM DFDL using the Cross Tester
//
//.settings(IBMDFDLCrossTesterPlugin.settings)

lazy val testIntegration =
  Project("daffodil-test-integration", file("daffodil-test-integration"))
    .dependsOn(cli % "test->test", udf % "test->test")
    .settings(commonSettings, nopublish)
    .settings(
      // CLI integration tests fork a new process which requires extra memory, so these should
      // only be run sequentially. We also need to stage the CLI script if any of the test
      // tasks are run
      Test / parallelExecution := false,
      Test / test := (Test / test).dependsOn(cli / Compile / stage).value,
      Test / testOnly := (Test / testOnly).dependsOn(cli / Compile / stage).evaluated,
      Test / testQuick := (Test / testQuick).dependsOn(cli / Compile / stage).evaluated,
    )

lazy val tutorials = Project("daffodil-tutorials", file("tutorials"))
  .dependsOn(tdmlProc % "test")
  .settings(commonSettings, nopublish)

lazy val testStdLayout = Project("daffodil-test-stdLayout", file("test-stdLayout"))
  .dependsOn(tdmlProc % "test")
  .settings(commonSettings, nopublish)

// Choices here are Java LTS versions, 8, 11, 17, 21,...
// However 8 is deprecated as of Java 21, so will be phased out.
val minSupportedJavaVersion: String =
  if (scala.util.Properties.isJavaAtLeast("21")) "11"
  else "8"

lazy val commonSettings = Seq(
  organization := "org.apache.daffodil",
  version := "3.7.0",
  scalaVersion := "2.12.19",
  crossScalaVersions := Seq("2.12.19"),
  scalacOptions ++= buildScalacOptions(scalaVersion.value),
  Compile / compile / javacOptions ++= buildJavacOptions(),
  logBuffered := true,
  transitiveClassifiers := Seq("sources", "javadoc"),
  retrieveManaged := true,
  useCoursier := false, // disabled because it breaks retrieveManaged (sbt issue #5078)
  exportJars := true,
  Test / exportJars := false,
  publishMavenStyle := true,
  Test / publishArtifact := false,
  ThisBuild / pomIncludeRepository := { _ => false },
  scmInfo := Some(
    ScmInfo(
      browseUrl = url("https://github.com/apache/daffodil"),
      connection = "scm:git:https://github.com/apache/daffodil",
    ),
  ),
  licenses := Seq(License.Apache2),
  homepage := Some(url("https://daffodil.apache.org")),
  releaseNotesURL := Some(url(s"https://daffodil.apache.org/releases/${version.value}/")),
  unmanagedBase := baseDirectory.value / "lib" / "jars",
  sourceManaged := baseDirectory.value / "src_managed",
  resourceManaged := baseDirectory.value / "resource_managed",
  libraryDependencies ++= Dependencies.common,
  testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "--verbosity=1"),
)

def buildScalacOptions(scalaVersion: String) = {
  val commonOptions = Seq(
    s"-release:$minSupportedJavaVersion", // scala 2.12 can only do Java 8, regardless of this setting.
    "-feature",
    "-deprecation",
    "-language:experimental.macros",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xxml:-coalescing",
    "-Xfuture",
    "-Ywarn-infer-any",
    "-Ywarn-inaccessible",
    // "-Ywarn-nullary-unit", // we cannot use this. It interferes with the Uniform Access Principle.
    // See https://stackoverflow.com/questions/7600910/difference-between-function-with-parentheses-and-without.
    "-Ywarn-unused-import",
  )

  val scalaVersionSpecificOptions = CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 12)) =>
      Seq(
        "-Ywarn-unused:imports",
      )
    case _ => Seq.empty
  }

  commonOptions ++ scalaVersionSpecificOptions
}

val javaVersionSpecificOptions = {
  val releaseOption = // as of Java 11, they no longer accept "-release". Must use "--release".
    if (scala.util.Properties.isJavaAtLeast("11")) "--release" else "-release"

  // Java 21 deprecates Java 8 and warns about it.
  // So if you are using Java 21, Java code compilation will specify a newer Java version
  // to avoid warnings.
  if (scala.util.Properties.isJavaAtLeast("11")) Seq(releaseOption, minSupportedJavaVersion)
  else if (scala.util.Properties.isJavaAtLeast("9")) Seq(releaseOption, "8")
  else Nil // for Java 8 compilation
}

// Workaround issue that some options are valid for javac, not javadoc.
// These javacOptions are for code compilation only. (Issue sbt/sbt#355)
def buildJavacOptions() = {
  val commonOptions = Seq(
    "-Werror",
    "-Xlint:deprecation",
  )

  commonOptions ++ javaVersionSpecificOptions
}

lazy val nopublish = Seq(
  publish := {},
  publishLocal := {},
  publishM2 := {},
  publish / skip := true,
)

// "usesMacros" is a list of settings that should be applied only to
// subprojects that use the Daffodil macroLib subproject. In addition to using
// these settings, projects that use macroLib should add it as
// "compile-internal" and "test-internal" dependency, so that the macroLib jar
// does not need to be published. For example:
//
//   lazy val subProject = Project(...)
//                           .dependsOn(..., macroLib % "compile-internal, test-internal")
//                           .settings(commonSettings, usesMacros)
//
lazy val usesMacros = Seq(
  // Because the macroLib is an internal dependency to projects that use this
  // setting, the macroLib is not published. But that means we need to copy the
  // macro src/bin into projects that use it, essentially inlining macros into
  // the projects that use them. This is standard practice according to:
  //
  //   https://www.scala-sbt.org/1.x/docs/Macro-Projects.html#Distribution
  //
  // Note that for packageBin, we only copy directories and class files--this
  // ignores files such a META-INFA/LICENSE and NOTICE that are duplicated and
  // would otherwise cause a conflict.
  Compile / packageBin / mappings ++= (macroLib / Compile / packageBin / mappings).value
    .filter { case (f, _) => f.isDirectory || f.getPath.endsWith(".class") },
  Compile / packageSrc / mappings ++= (macroLib / Compile / packageSrc / mappings).value,
)

lazy val libManagedSettings = Seq(
  genManaged := {
    (Compile / managedSources).value
    (Compile / managedResources).value
  },
  Compile / genProps := {
    val cp = (propgen / Runtime / dependencyClasspath).value
    val inSrc = (propgen / Runtime / sources).value
    val inRSrc = (propgen / Compile / resources).value
    val stream = (propgen / streams).value
    val outdir = (Compile / sourceManaged).value
    val mainClass = "org.apache.daffodil.propGen.PropertyGenerator"
    val args = Seq(mainClass, outdir.toString)
    val filesToWatch = (inSrc ++ inRSrc).toSet
    val cachedFun = FileFunction.cached(stream.cacheDirectory / "propgen") { _ =>
      val forkCaptureLogger = ForkCaptureLogger()
      val forkOpts = ForkOptions()
        .withOutputStrategy(Some(LoggedOutput(forkCaptureLogger)))
        .withBootJars(cp.files.toVector)
      val ret = Fork.java(forkOpts, args)
      forkCaptureLogger.stderr.foreach { stream.log.error(_) }
      if (ret != 0) {
        sys.error("Failed to generate code")
      }
      val files = forkCaptureLogger.stdout.map { f =>
        new File(f)
      }.toSet
      stream.log.info(s"generated ${files.size} Scala sources to $outdir")
      files
    }
    cachedFun(filesToWatch).toSeq
  },
  Compile / genSchemas := {
    val inRSrc = (propgen / Compile / resources).value
    val stream = (propgen / streams).value
    val outdir = (Compile / resourceManaged).value
    val filesToWatch = inRSrc.filter { _.isFile }.toSet
    val cachedFun =
      FileFunction.cached(stream.cacheDirectory / "schemasgen") { (schemas: Set[File]) =>
        val files = schemas.map { schema =>
          val out = outdir / "org" / "apache" / "daffodil" / "xsd" / schema.getName
          IO.copyFile(schema, out)
          out
        }
        stream.log.info(s"generated ${files.size} XML schemas to $outdir")
        files
      }
    cachedFun(filesToWatch).toSeq
  },
  Compile / genVersion := {
    val resourceDir = (Compile / resourceManaged).value
    val outFile = resourceDir / "org" / "apache" / "daffodil" / "lib" / "VERSION"
    if (!outFile.exists || IO.read(outFile) != version.value) {
      // only write the VERSION file if the version has changed. If we always write, then the
      // mtime changes and sbt thinks it needs to rebuild everything since a resource changed.
      IO.write(outFile, version.value)
    }
    Seq(outFile)
  },
  Compile / sourceGenerators ++= Seq(
    (Compile / genProps).taskValue,
  ),
  Compile / resourceGenerators ++= Seq(
    (Compile / genSchemas).taskValue,
    (Compile / genVersion).taskValue,
  ),
)

lazy val ratSettings = Seq(
  ratLicenses := Seq(
    ("BSD2 ", Rat.BSD2_LICENSE_NAME, Rat.LICENSE_TEXT_PASSERA),
  ),
  ratLicenseFamilies := Seq(
    Rat.BSD2_LICENSE_NAME,
  ),
  ratExcludes := Rat.excludes,
  ratFailBinaries := true,
)

lazy val unidocSettings = Seq(
  ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(sapi, udf),
  ScalaUnidoc / unidoc / scalacOptions := Seq(
    "-doc-title",
    "Apache Daffodil " + version.value + " Scala API",
    "-doc-root-content",
    (sapi / baseDirectory).value + "/root-doc.txt",
  ),
  JavaUnidoc / unidoc / unidocProjectFilter := inProjects(japi, udf),
  JavaUnidoc / unidoc / javacOptions := Seq(
    "-windowtitle",
    "Apache Daffodil " + version.value + " Java API",
    "-doctitle",
    "<h1>Apache Daffodil " + version.value + " Java API</h1>",
    "-notimestamp",
    "-quiet",
  ),
  JavaUnidoc / unidoc / unidocAllSources := (JavaUnidoc / unidoc / unidocAllSources).value.map {
    sources =>
      sources.filterNot { source =>
        source.toString.contains("$") || source.toString.contains("packageprivate")
      }
  },
)

lazy val genCExamplesSettings = Seq(
  Compile / genCExamples := {
    val cp = (codeGenC / Test / dependencyClasspath).value
    val inSrc = (codeGenC / Compile / sources).value
    val inRSrc = (codeGenC / Test / resources).value
    val stream = (codeGenC / streams).value
    val filesToWatch = (inSrc ++ inRSrc).toSet
    val cachedFun = FileFunction.cached(stream.cacheDirectory / "genCExamples") { _ =>
      val forkCaptureLogger = ForkCaptureLogger()
      val forkOpts = ForkOptions()
        .withOutputStrategy(Some(LoggedOutput(forkCaptureLogger)))
        .withBootJars(cp.files.toVector)
      val mainClass = "org.apache.daffodil.codegen.c.DaffodilCExamplesGenerator"
      val outdir = (codeGenC / Test / sourceDirectory).value / "examples"
      val args = Seq(mainClass, outdir.toString)
      val ret = Fork.java(forkOpts, args)
      forkCaptureLogger.stderr.foreach { stream.log.error(_) }
      if (ret != 0) {
        sys.error("failed to generate C example files")
      }
      val files = forkCaptureLogger.stdout
        .filterNot(_.startsWith("WARNING"))
        .map { f =>
          new File(f)
        }
        .toSet
      stream.log.info(s"generated ${files.size} C examples to $outdir")
      files
    }
    cachedFun(filesToWatch).toSeq
  },
  Compile / compile := {
    val res = (Compile / compile).value
    (Compile / genCExamples).value
    res
  },
)
