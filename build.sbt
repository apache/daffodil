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

import sbtcc._

import scala.collection.immutable.ListSet

// Silence an errant sbt linter warning about unused sbt settings. For some
// reason, the sbt linter thinks the below settings are set but not used, which
// leads to a bunch of noisy warnings. But they clearly are used. Seems to be a
// bug in the linter where it cannot detect that some keys are used. The
// following is the sbt recommended way to silence these linter warnings on a
// per setting basis rather thand disabling the linter completely.
Global / excludeLintKeys ++= Set(
  EclipseKeys.classpathTransformerFactories,
)

lazy val genManaged = taskKey[Seq[File]]("Generate managed sources and resources")
lazy val genProps = taskKey[Seq[File]]("Generate properties scala source")
lazy val genSchemas = taskKey[Seq[File]]("Generated DFDL schemas")
lazy val genExamples = taskKey[Seq[File]]("Generate runtime2 example files")

lazy val daffodil         = project.in(file(".")).configs(IntegrationTest)
                              .enablePlugins(JavaUnidocPlugin, ScalaUnidocPlugin)
                              .aggregate(macroLib, propgen, lib, io, runtime1, runtime1Unparser, runtime1Layers, runtime2, core, japi, sapi, tdmlLib, tdmlProc, cli, udf, schematron, test, testIBM1, tutorials, testStdLayout)
                              .settings(commonSettings, nopublish, ratSettings, unidocSettings, genExamplesSettings)

lazy val macroLib         = Project("daffodil-macro-lib", file("daffodil-macro-lib")).configs(IntegrationTest)
                              .settings(commonSettings, nopublish)
                              .settings(libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value)

lazy val propgen          = Project("daffodil-propgen", file("daffodil-propgen")).configs(IntegrationTest)
                              .settings(commonSettings, nopublish)

lazy val lib              = Project("daffodil-lib", file("daffodil-lib")).configs(IntegrationTest)
                              .dependsOn(macroLib % "compile-internal, test-internal")
                              .settings(commonSettings, libManagedSettings, usesMacros)

lazy val io               = Project("daffodil-io", file("daffodil-io")).configs(IntegrationTest)
                              .dependsOn(lib, macroLib % "compile-internal, test-internal")
                              .settings(commonSettings, usesMacros)

lazy val runtime1         = Project("daffodil-runtime1", file("daffodil-runtime1")).configs(IntegrationTest)
                              .dependsOn(io, lib % "test->test", udf, macroLib % "compile-internal, test-internal")
                              .settings(commonSettings, usesMacros)

lazy val runtime1Unparser = Project("daffodil-runtime1-unparser", file("daffodil-runtime1-unparser")).configs(IntegrationTest)
                              .dependsOn(runtime1, lib % "test->test", runtime1 % "test->test", runtime1Layers)
                              .settings(commonSettings)

lazy val runtime1Layers = Project("daffodil-runtime1-layers", file("daffodil-runtime1-layers")).configs(IntegrationTest)
                              .dependsOn(runtime1, lib % "test->test")
                              .settings(commonSettings)

val runtime2CFiles        = Library("libruntime2.a")
lazy val runtime2         = Project("daffodil-runtime2", file("daffodil-runtime2")).configs(IntegrationTest)
                              .enablePlugins(CcPlugin)
                              .dependsOn(core, core % "test->test")
                              .settings(commonSettings)
                              .settings(
                                Compile / cCompiler := sys.env.getOrElse("CC", "cc"),
                                Compile / ccArchiveCommand := sys.env.getOrElse("AR", "ar"),
                                Compile / ccTargets := ListSet(runtime2CFiles),
                                Compile / cSources  := Map(
                                  runtime2CFiles -> ((Compile / resourceDirectory).value / "org" / "apache" / "daffodil" / "runtime2" / "c"
                                    * GlobFilter("lib*") * GlobFilter("*.c")).get()
                                ),
                                Compile / cIncludeDirectories := Map(
                                  runtime2CFiles -> Seq(
                                    (Compile / resourceDirectory).value / "org" / "apache" / "daffodil" / "runtime2" / "c" / "libcli",
                                    (Compile / resourceDirectory).value / "org" / "apache" / "daffodil" / "runtime2" / "c" / "libruntime",
                                  )
                                ),
                                Compile / cFlags := (Compile / cFlags).value.withDefaultValue(Seq("-Wall", "-Wextra", "-Wpedantic", "-std=gnu11"))
                              )

lazy val core             = Project("daffodil-core", file("daffodil-core")).configs(IntegrationTest)
                              .dependsOn(runtime1Unparser, udf, lib % "test->test", runtime1 % "test->test", io % "test->test")
                              .settings(commonSettings)

lazy val japi             = Project("daffodil-japi", file("daffodil-japi")).configs(IntegrationTest)
                              .dependsOn(core)
                              .settings(commonSettings)

lazy val sapi             = Project("daffodil-sapi", file("daffodil-sapi")).configs(IntegrationTest)
                              .dependsOn(core)
                              .settings(commonSettings)

lazy val tdmlLib             = Project("daffodil-tdml-lib", file("daffodil-tdml-lib")).configs(IntegrationTest)
                              .dependsOn(macroLib % "compile-internal", lib, io, io % "test->test")
                              .settings(commonSettings)

lazy val tdmlProc         = Project("daffodil-tdml-processor", file("daffodil-tdml-processor")).configs(IntegrationTest)
                              .dependsOn(tdmlLib, runtime2, core)
                              .settings(commonSettings)

lazy val cli              = Project("daffodil-cli", file("daffodil-cli")).configs(IntegrationTest)
                              .dependsOn(tdmlProc, runtime2, sapi, japi, schematron % Runtime, udf % "it->test") // causes runtime2/sapi/japi to be pulled into the helper zip/tar
                              .settings(commonSettings, nopublish)
                              .settings(libraryDependencies ++= Dependencies.cli)
                              .settings(libraryDependencies ++= Dependencies.exi)

lazy val udf              = Project("daffodil-udf", file("daffodil-udf")).configs(IntegrationTest)
                              .settings(commonSettings)

lazy val schematron       = Project("daffodil-schematron", file("daffodil-schematron"))
                              .dependsOn(lib, sapi % Test)
                              .settings(commonSettings)
                              .settings(libraryDependencies ++= Dependencies.schematron)
                              .configs(IntegrationTest)

lazy val test             = Project("daffodil-test", file("daffodil-test")).configs(IntegrationTest)
                              .dependsOn(tdmlProc, runtime2 % "test->test", udf % "test->test")
                              .settings(commonSettings, nopublish)
                              //
                              // Uncomment the following line to run these tests 
                              // against IBM DFDL using the Cross Tester
                              //
                              //.settings(IBMDFDLCrossTesterPlugin.settings)

lazy val testIBM1         = Project("daffodil-test-ibm1", file("daffodil-test-ibm1")).configs(IntegrationTest)
                              .dependsOn(tdmlProc)
                              .settings(commonSettings, nopublish)
                              //
                              // Uncomment the following line to run these tests 
                              // against IBM DFDL using the Cross Tester
                              //
                              //.settings(IBMDFDLCrossTesterPlugin.settings)

lazy val tutorials        = Project("daffodil-tutorials", file("tutorials")).configs(IntegrationTest)
                              .dependsOn(tdmlProc)
                              .settings(commonSettings, nopublish)

lazy val testStdLayout    = Project("daffodil-test-stdLayout", file("test-stdLayout")).configs(IntegrationTest)
                              .dependsOn(tdmlProc)
                              .settings(commonSettings, nopublish)


lazy val commonSettings = Seq(
  organization := "org.apache.daffodil",
  version := "3.5.0-SNAPSHOT",
  scalaVersion := "2.12.17",
  crossScalaVersions := Seq("2.12.17"),
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
      connection = "scm:git:https://github.com/apache/daffodil"
    )
  ),
  licenses := Seq(License.Apache2),
  homepage := Some(url("https://daffodil.apache.org")),
  unmanagedBase := baseDirectory.value / "lib" / "jars",
  sourceManaged := baseDirectory.value / "src_managed",
  resourceManaged := baseDirectory.value / "resource_managed",
  libraryDependencies ++= Dependencies.common,
  IntegrationTest / parallelExecution := false,
  testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "--verbosity=1"),
) ++ Defaults.itSettings


def buildScalacOptions(scalaVersion: String) = {
  val commonOptions = Seq(
    "-target:jvm-1.8",
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
    "-Ywarn-unused-import"
  )

  val scalaVersionSpecificOptions = CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 12)) => Seq(
      "-Ywarn-unused:imports"
    )
    case _ => Seq.empty
  }

  val javaVersionSpecificOptions =
    if (scala.util.Properties.isJavaAtLeast("9"))
      Seq("-release", "8") // ensure Java backwards compatibility (DAFFODIL-2579)
    else
      Seq.empty

    commonOptions ++ scalaVersionSpecificOptions ++ javaVersionSpecificOptions
}

// Workaround issue that some options are valid for javac, not javadoc.
// These javacOptions are for code compilation only. (Issue sbt/sbt#355)
def buildJavacOptions() = {
  val commonOptions = Seq(
    "-Werror",
    "-Xlint:deprecation"
  )

  val javaVersionSpecificOptions =
    if (scala.util.Properties.isJavaAtLeast("9"))
      Seq("--release", "8") // ensure Java backwards compatibility (DAFFODIL-2579)
    else
      Seq.empty

  commonOptions ++ javaVersionSpecificOptions
}

lazy val nopublish = Seq(
  publish := {},
  publishLocal := {},
  publishM2 := {},
  publish / skip := true
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
  Compile / packageBin / mappings ++= (macroLib / Compile / packageBin / mappings).value.filter { case (f, _) => f.isDirectory || f.getPath.endsWith(".class") },
  Compile / packageSrc / mappings ++= (macroLib / Compile / packageSrc / mappings).value,

  // The .classpath files that the sbt eclipse plugin creates need minor
  // modifications. Fortunately, the plugin allows us to provide "transformers"
  // to make such modifications. Note that because this is part of the
  // "usesMacro" setting, the following transformations are only applied to
  // .classpath files in projects that use macros and add this setting.
  EclipseKeys.classpathTransformerFactories ++= Seq(
    // The macroLib project needs to be a "compile-internal" dependency to
    // projects that add this "usesMacros" setting. But the sbt eclipse plugin
    // only looks at "compile" dependencies when building .classpath files.
    // This means that eclipse projects that use macros don't have a dependency
    // to macroLib and so fail to compile. This transformation looks for
    // "classpath" nodes, and appends a new "classpathentry" node as a child
    // referencing the macroLib project. This causes Eclipse to treat macroLib
    // just like any other dependency to allow compilation to work.
    transformNode("classpath", DefaultTransforms.Append(EclipseClasspathEntry.Project(macroLib.base.toString))),
  ),
)

lazy val libManagedSettings = Seq(
  genManaged := {
    (Compile / genProps).value ++ (Compile / genSchemas).value
  },
  Compile / genProps := {
    val cp = (propgen / Runtime / dependencyClasspath).value
    val inSrc = (propgen / Runtime/ sources).value
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
      stream.log.info(s"generated ${files.size} Scala sources to ${outdir}")
      files
    }
    cachedFun(filesToWatch).toSeq
  },
  Compile / genSchemas := {
    val inRSrc = (propgen / Compile / resources).value
    val stream = (propgen / streams).value
    val outdir = (Compile / resourceManaged).value
    val filesToWatch = inRSrc.filter{_.isFile}.toSet
    val cachedFun = FileFunction.cached(stream.cacheDirectory / "schemasgen") { (schemas: Set[File]) =>
      val files = schemas.map { schema =>
        val out = outdir / "org" / "apache" / "daffodil" / "xsd" / schema.getName
        IO.copyFile(schema, out)
        out
      }
      stream.log.info(s"generated ${files.size} XML schemas to ${outdir}")
      files
    }
    cachedFun(filesToWatch).toSeq
  },
  Compile / sourceGenerators += (Compile / genProps).taskValue,
  Compile / resourceGenerators += (Compile / genSchemas).taskValue
)

lazy val ratSettings = Seq(
  ratLicenses := Seq(
    ("BSD2 ", Rat.BSD2_LICENSE_NAME, Rat.LICENSE_TEXT_PASSERA)
  ),
  ratLicenseFamilies := Seq(
    Rat.BSD2_LICENSE_NAME
  ),
  ratExcludes := Rat.excludes,
  ratFailBinaries := true,
)

lazy val unidocSettings = Seq(
  ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(sapi, udf),
  ScalaUnidoc / unidoc / scalacOptions := Seq(
    "-doc-title", "Apache Daffodil " + version.value + " Scala API",
    "-doc-root-content", (sapi / baseDirectory).value + "/root-doc.txt"
  ),

  JavaUnidoc / unidoc / unidocProjectFilter := inProjects(japi, udf),
  JavaUnidoc / unidoc / javacOptions:= Seq(
    "-windowtitle", "Apache Daffodil " + version.value + " Java API",
    "-doctitle", "<h1>Apache Daffodil " + version.value + " Java API</h1>",
    "-notimestamp",
    "-quiet",
  ),
  JavaUnidoc / unidoc / unidocAllSources := (JavaUnidoc / unidoc / unidocAllSources).value.map { sources =>
    sources.filterNot { source =>
      source.toString.contains("$") || source.toString.contains("packageprivate")
    }
  },
)

lazy val genExamplesSettings = Seq(
  Compile / genExamples := {
    val cp = (runtime2 / Test / dependencyClasspath).value
    val inSrc = (runtime2 / Compile / sources).value
    val inRSrc = (runtime2 / Test / resources).value
    val stream = (runtime2 / streams).value
    val filesToWatch = (inSrc ++ inRSrc).toSet
    val cachedFun = FileFunction.cached(stream.cacheDirectory / "genExamples") { _ =>
      val forkCaptureLogger = ForkCaptureLogger()
      val forkOpts = ForkOptions()
                       .withOutputStrategy(Some(LoggedOutput(forkCaptureLogger)))
                       .withBootJars(cp.files.toVector)
      val mainClass = "org.apache.daffodil.runtime2.CodeGenerator"
      val outdir = (runtime2 / Test / sourceDirectory).value / "c" / "examples"
      val args = Seq(mainClass, outdir.toString)
      val ret = Fork.java(forkOpts, args)
      forkCaptureLogger.stderr.foreach { stream.log.error(_) }
      if (ret != 0) {
        sys.error("failed to generate example files")
      }
      val files = forkCaptureLogger.stdout.filterNot(_.startsWith("WARNING")).map { f =>
        new File(f)
      }.toSet
      stream.log.info(s"generated ${files.size} runtime2 example files to ${outdir}")
      files
    }
    cachedFun(filesToWatch).toSeq
  },
  Compile / compile := {
    val res = (Compile / compile).value
    (Compile / genExamples).value
    res
  }
)
