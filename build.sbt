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

lazy val genManaged = taskKey[Unit]("Generate managed sources and resources")
lazy val genProps = taskKey[Seq[File]]("Generate properties scala source")
lazy val genSchemas = taskKey[Seq[File]]("Generated DFDL schemas")


lazy val daffodil         = Project("daffodil", file(".")).configs(IntegrationTest)
                              .aggregate(macroLib, propgen, lib, io, runtime1, runtime1Unparser, core, japi, sapi, tdmlLib, tdmlProc, cli, test, testIBM1, tutorials, testStdLayout)
                              .settings(commonSettings, nopublish, ratSettings)

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
                              .dependsOn(io, lib % "test->test")
                              .settings(commonSettings)

lazy val runtime1Unparser = Project("daffodil-runtime1-unparser", file("daffodil-runtime1-unparser")).configs(IntegrationTest)
                              .dependsOn(runtime1, lib % "test->test", runtime1 % "test->test")
                              .settings(commonSettings)

lazy val core             = Project("daffodil-core", file("daffodil-core")).configs(IntegrationTest)
                              .dependsOn(runtime1Unparser, lib % "test->test", runtime1 % "test->test")
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
                              .dependsOn(tdmlLib, core)
                              .settings(commonSettings)

lazy val cli              = Project("daffodil-cli", file("daffodil-cli")).configs(IntegrationTest)
                              .dependsOn(tdmlProc, sapi, japi) // causes sapi/japi to be pulled in to the helper zip/tar
                              .settings(commonSettings, nopublish)
                              .settings(libraryDependencies ++= Dependencies.cli) 

lazy val test             = Project("daffodil-test", file("daffodil-test")).configs(IntegrationTest)
                              .dependsOn(tdmlProc)
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
  version := "2.4.0-SNAPSHOT",
  scalaVersion := "2.12.6",
  crossScalaVersions := Seq("2.12.6", "2.11.12"),
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-language:experimental.macros",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xxml:-coalescing",
  ),
  logBuffered := true,
  transitiveClassifiers := Seq("sources", "javadoc"),
  retrieveManaged := true,
  exportJars := true,
  exportJars in Test := false,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository in ThisBuild := { _ => false },
  scmInfo := Some(
    ScmInfo(
      browseUrl = url("https://gitbox.apache.org/repos/asf/incubator-daffodil.git"),
      connection = "scm:git:https://gitbox.apache.org/repos/asf/incubator-daffodil.git"
    )
  ),
  licenses := Seq("Apache License, Version 2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
  mappings in (Compile, packageBin) += baseDirectory.value / ".." / "LICENSE" -> "META-INF/LICENSE",
  mappings in (Compile, packageBin) += baseDirectory.value / ".." / "NOTICE" -> "META-INF/NOTICE",
  homepage := Some(url("https://daffodil.apache.org")),
  unmanagedBase := baseDirectory.value / "lib" / "jars",
  sourceManaged := baseDirectory.value / "src_managed",
  resourceManaged := baseDirectory.value / "resource_managed",
  libraryDependencies ++= Dependencies.common,
  parallelExecution in IntegrationTest := false
) ++ Defaults.itSettings

lazy val nopublish = Seq(
  publish := {},
  publishLocal := {},
  publishM2 := {},
  skip in publish := true
)

lazy val usesMacros = Seq(
  // copies classe and source files into the project that uses macros. Note
  // that for packageBin, we only copy directories and class files--this
  // ignores files such a META-INFA/LICENSE and NOTICE that are duplicated and
  // would otherwise cause a conflict
  mappings in (Compile, packageBin) ++= mappings.in(macroLib, Compile, packageBin).value.filter { case (f, _) => f.isDirectory || f.getPath.endsWith(".class") },
  mappings in (Compile, packageSrc) ++= mappings.in(macroLib, Compile, packageSrc).value
)

lazy val libManagedSettings = Seq(
  genManaged := {
    (genProps in Compile).value
    (genSchemas in Compile).value
  },
  genProps in Compile := {
    val cp = (dependencyClasspath in Runtime in propgen).value
    val inSrc = (sources in Compile in propgen).value
    val inRSrc = (resources in Compile in propgen).value
    val stream = (streams in propgen).value
    val outdir = (sourceManaged in Compile).value
    val filesToWatch = (inSrc ++ inRSrc).toSet
    val cachedFun = FileFunction.cached(stream.cacheDirectory / "propgen") { (in: Set[File]) =>
      val mainClass = "org.apache.daffodil.propGen.PropertyGenerator"
      val out = new java.io.ByteArrayOutputStream()
      val forkOpts = ForkOptions()
                       .withOutputStrategy(Some(CustomOutput(out)))
                       .withBootJars(cp.files.toVector)
      val ret = new Fork("java", Some(mainClass)).fork(forkOpts, Seq(outdir.toString)).exitValue()
      if (ret != 0) {
        sys.error("Failed to generate code")
      }
      val bis = new java.io.ByteArrayInputStream(out.toByteArray)
      val isr = new java.io.InputStreamReader(bis)
      val br = new java.io.BufferedReader(isr)
      val iterator = Iterator.continually(br.readLine()).takeWhile(_ != null)
      val files = iterator.map { f =>
        stream.log.info("Generated %s".format(f))
        new File(f)
      }.toSet
      files
    }
    cachedFun(filesToWatch).toSeq
  },
  genSchemas in Compile := {
    val inRSrc = (resources in Compile in propgen).value
    val stream = (streams in propgen).value
    val outdir = (resourceManaged in Compile).value
    val filesToWatch = inRSrc.filter{_.isFile}.toSet
    val cachedFun = FileFunction.cached(stream.cacheDirectory / "schemasgen") { (schemas: Set[File]) =>
      schemas.map { schema =>
        val out = outdir / "org" / "apache" / "daffodil" / "xsd" / schema.getName
        IO.copyFile(schema, out)
        stream.log.info("Generated %s".format(out))
        out
      }
    }
    cachedFun(filesToWatch).toSeq
  },
  sourceGenerators in Compile += (genProps in Compile).taskValue,
  resourceGenerators in Compile += (genSchemas in Compile).taskValue
)

lazy val ratSettings = Seq(
  ratLicenses := Seq(
    ("BSD2 ", Rat.BSD2_LICENSE_NAME, Rat.LICENSE_TEXT_PASSERA),
    ("BSD3 ", Rat.BSD3_LICENSE_NAME, Rat.LICENSE_TEXT_SCALA)
  ),

  ratLicenseFamilies := Seq(
    Rat.BSD3_LICENSE_NAME,
    Rat.BSD2_LICENSE_NAME
  ),

  ratExcludes := Rat.excludes
)

