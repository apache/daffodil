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

import sbtcc.*

lazy val genManaged = taskKey[Unit]("Generate managed sources and resources")
lazy val genProps = taskKey[Seq[File]]("Generate properties scala source")
lazy val genSchemas = taskKey[Seq[File]]("Generate DFDL schemas")
lazy val genCExamples = taskKey[Seq[File]]("Generate C example files")
lazy val genVersion = taskKey[Seq[File]]("Generate VERSION file")
lazy val genTunablesDoc = taskKey[Seq[File]]("Generate tunables doc from dafext.xsd file")

lazy val daffodil = project
  .in(file("."))
  .enablePlugins(ScalaUnidocPlugin)
  .aggregate(
    cli,
    codeGenC,
    macroLib,
    propgen,
    core,
    schematron,
    slf4jLogger,
    tdmlJunit,
    tdmlLib,
    tdmlProc,
    testDaf,
    testIBM1,
    // testIntegration, // integration tests must be run manually
    testStdLayout,
    tutorials
  )
  .settings(
    commonSettings,
    nopublish,
    ratSettings,
    unidocSettings,
    genTunablesDocSettings,
    genCExamplesSettings
  )

lazy val macroLib = Project("daffodil-macro-lib", file("daffodil-macro-lib"))
  .settings(commonSettings, nopublish)
  .disablePlugins(OsgiCheckPlugin)

lazy val propgen = Project("daffodil-propgen", file("daffodil-propgen"))
  .settings(commonSettings, nopublish)

lazy val slf4jLogger = Project("daffodil-slf4j-logger", file("daffodil-slf4j-logger"))
  .settings(commonSettings)
  .settings(libraryDependencies ++= Dependencies.slf4jAPI)

lazy val core = Project("daffodil-core", file("daffodil-core"))
  .dependsOn(
    macroLib % "compile-internal, test-internal",
    slf4jLogger % "test"
  )
  .settings(commonSettings, generatorSettings, usesMacros)

val codeGenCLib = Library("libruntime.a")
lazy val codeGenC = Project("daffodil-codegen-c", file("daffodil-codegen-c"))
  .enablePlugins(CcPlugin)
  .dependsOn(
    core,
    core % "test->test",
    slf4jLogger % "test"
  )
  .settings(commonSettings)
  .settings(
    Compile / cCompiler := sys.env.getOrElse("CC", "cc"),
    Compile / ccArchiveCommand := sys.env.getOrElse("AR", "ar"),
    Compile / ccTargets := ListSet(codeGenCLib),
    Compile / cSources := Map(
      codeGenCLib -> ((Compile / resourceDirectory).value / "org" / "apache" / "daffodil" / "codegen" / "c" / "files"
        * GlobFilter("lib*") * GlobFilter("*.c")).get()
    ),
    Compile / cIncludeDirectories := Map(
      codeGenCLib -> Seq(
        (Compile / resourceDirectory).value / "org" / "apache" / "daffodil" / "codegen" / "c" / "files" / "libcli",
        (Compile / resourceDirectory).value / "org" / "apache" / "daffodil" / "codegen" / "c" / "files" / "libruntime"
      )
    ),
    Compile / cFlags := (Compile / cFlags).value
      .withDefaultValue(Seq("-Wall", "-Wextra", "-Wpedantic", "-std=gnu11"))
  )

lazy val tdmlLib = Project("daffodil-tdml-lib", file("daffodil-tdml-lib"))
  .dependsOn(
    macroLib % "compile-internal",
    core,
    core % "test->test",
    slf4jLogger % "test"
  )
  .settings(commonSettings)

lazy val tdmlProc = Project("daffodil-tdml-processor", file("daffodil-tdml-processor"))
  .dependsOn(tdmlLib, codeGenC, core, slf4jLogger)
  .settings(commonSettings)

lazy val tdmlJunit = Project("daffodil-tdml-junit", file("daffodil-tdml-junit"))
  .dependsOn(tdmlProc)
  .settings(commonSettings)
  .settings(libraryDependencies ++= Dependencies.junit)

lazy val cli = Project("daffodil-cli", file("daffodil-cli"))
  .dependsOn(
    tdmlProc,
    codeGenC,
    core,
    schematron % Runtime,
    slf4jLogger
  ) // causes codegen-c to be pulled into the helper zip/tar
  .settings(commonSettings, nopublish)
  .settings(libraryDependencies ++= Dependencies.cli)
  .settings(libraryDependencies ++= Dependencies.exi)

lazy val schematron = Project("daffodil-schematron", file("daffodil-schematron"))
  .dependsOn(core, core % Test, slf4jLogger % "test")
  .settings(commonSettings)
  .settings(libraryDependencies ++= Dependencies.schematron)

lazy val testDaf = Project("daffodil-test", file("daffodil-test"))
  .dependsOn(tdmlJunit % "test", codeGenC % "test->test", core % "test->test")
  .settings(commonSettings, nopublish)
//
// Uncomment the following line to run these tests
// against IBM DFDL using the Cross Tester
//
//.settings(IBMDFDLCrossTesterPlugin.settings)

lazy val testIBM1 = Project("daffodil-test-ibm1", file("daffodil-test-ibm1"))
  .dependsOn(tdmlJunit % "test")
  .settings(commonSettings, nopublish)
//
// Uncomment the following line to run these tests
// against IBM DFDL using the Cross Tester
//
//.settings(IBMDFDLCrossTesterPlugin.settings)

lazy val testIntegration =
  Project("daffodil-test-integration", file("daffodil-test-integration"))
    .dependsOn(cli % "test->test", core % "test->test", testDaf % "test->test")
    .settings(commonSettings, nopublish)
    .settings(
      // CLI integration tests fork a new process which requires extra memory, so these should
      // only be run sequentially. We also need to stage the CLI script if any of the test
      // tasks are run
      Test / parallelExecution := false,
      Test / test := (Test / test).dependsOn(cli / Compile / stage).value,
      Test / testOnly := (Test / testOnly).dependsOn(cli / Compile / stage).evaluated,
      Test / testQuick := (Test / testQuick).dependsOn(cli / Compile / stage).evaluated
    )

lazy val tutorials = Project("daffodil-tutorials", file("tutorials"))
  .dependsOn(tdmlJunit % "test")
  .settings(commonSettings, nopublish)

lazy val testStdLayout = Project("daffodil-test-stdLayout", file("test-stdLayout"))
  .dependsOn(tdmlJunit % "test")
  .settings(commonSettings, nopublish)

// Choices here are Java LTS versions, 17, 21,...
val minSupportedJavaVersion: String = "17"

lazy val commonSettings = Seq(
  organization := "org.apache.daffodil",
  version := IO.read((ThisBuild / baseDirectory).value / "VERSION").trim,
  scalaVersion := "3.3.7",
  crossScalaVersions := Seq("3.3.7"),
  scalacOptions ++= buildScalacOptions(scalaVersion.value),
  Test / scalacOptions ++= buildTestScalacOptions(scalaVersion.value),
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
  releaseNotesURL := Some(url(s"https://daffodil.apache.org/releases/${version.value}/")),
  unmanagedBase := baseDirectory.value / "lib" / "jars",
  sourceManaged := baseDirectory.value / "src_managed",
  resourceManaged := baseDirectory.value / "resource_managed",
  libraryDependencies ++= Dependencies.common,
  testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "--verbosity=1"),
  Compile / packageDoc / publishArtifact := false
)

def buildScalacOptions(scalaVersion: String) = {
  val commonOptions = Seq(
    s"-release:$minSupportedJavaVersion",
    "-feature",
    "-deprecation",
    "-unchecked"
  )

  val scalaVersionSpecificOptions = CrossVersion.partialVersion(scalaVersion) match {
    case Some((3, _)) =>
      Seq(
        "-no-indent",
        "-Werror",
        "-Wunused:imports"
      )
    case _ => Seq.empty
  }

  commonOptions ++ scalaVersionSpecificOptions
}

def buildTestScalacOptions(scalaVersion: String) = {
  val commonOptions = Seq.empty

  val scalaVersionSpecificOptions = CrossVersion.partialVersion(scalaVersion) match {
    case Some((3, _)) => Seq.empty
    case _ => Seq.empty
  }

  commonOptions ++ scalaVersionSpecificOptions
}

// Workaround issue that some options are valid for javac, not javadoc.
// These javacOptions are for code compilation only. (Issue sbt/sbt#355)
def buildJavacOptions() = {
  val commonOptions = Seq(
    "-Werror",
    "-Xlint:deprecation",
    "-deprecation",
    "-Xlint:dep-ann",
    "-Xlint:unchecked",
    "--release",
    minSupportedJavaVersion
  )

  commonOptions
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
  Compile / packageBin / mappings ++= (macroLib / Compile / packageBin / mappings).value
    .filter { case (f, _) => f.isDirectory || f.getPath.endsWith(".class") },
  Compile / packageSrc / mappings ++= (macroLib / Compile / packageSrc / mappings).value
)

lazy val generatorSettings = Seq(
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
    (Compile / genProps).taskValue
  ),
  Compile / resourceGenerators ++= Seq(
    (Compile / genSchemas).taskValue,
    (Compile / genVersion).taskValue
  )
)

lazy val ratSettings = Seq(
  ratLicenses := Seq(
    ("BSD2 ", Rat.BSD2_LICENSE_NAME, Rat.LICENSE_TEXT_PASSERA)
  ),
  ratLicenseFamilies := Seq(
    Rat.BSD2_LICENSE_NAME
  ),
  ratExcludes := Rat.excludes,
  ratFailBinaries := true
)

/**
 * For maximum compatability with Java users, our API is written entirely in Java. The
 * following configures unidoc to convert all our .java files to javadoc
 *
 * Note that that we do not use JavaUnidoc because that uses the genjavadoc plugin to
 * convert .scala files to .java and runs javadoc on the result. Our API is already all
 * .java so we do not need this converstion.
 *
 * Instead, we use ScalaUnidoc to generate javadoc. This works because all of our API sources
 * are .java files, which causes unidoc to generate documentation using javadoc instead of
 * scaladoc. Note that we need to change the logic of unidocAllSources because by default it
 * only includes .tasty files when run with Scala 3--we change it to instead find scoped source
 * files (i.e. .scala and .java files) and filter to include only .java files.
 *
 * Note that Scala 3 does not support .java files, so we are unlikely to ever be able to
 * generate scaladoc-style output in addition to the javadoc-style currently supported.
 */
lazy val unidocSettings =
  Seq(
    ScalaUnidoc / unidoc / unidocAllClasspaths := Seq(
      (core / Compile / fullClasspath).value,
      (Compile / fullClasspath).value
    ),
    ScalaUnidoc / unidoc / unidocProjectFilter :=
      inProjects(core),
    ScalaUnidoc / unidoc / javacOptions := Seq(
      "-windowtitle",
      "Apache Daffodil " + version.value + " API",
      "-doctitle",
      "<h1>Apache Daffodil " + version.value + " API</h1>",
      "-notimestamp",
      "-quiet"
    ),
    ScalaUnidoc / unidoc / unidocAllSources := Def
      .taskDyn {
        val scopeFilter = (ScalaUnidoc / unidoc / unidocScopeFilter).value
        val scopedSources = sources.all(scopeFilter)
        scopedSources
      }
      .value
      .map(
        _.filter(fn =>
          fn.getName.endsWith(".java") && fn.getPath.contains("org/apache/daffodil/api")
        )
      )
  )

lazy val genTunablesDocSettings = Seq(
  Compile / genTunablesDoc := {
    val stream = (propgen / streams).value
    val dafExtFile =
      (propgen / Compile / resources).value.find(_.getName == "dafext.xsd").get
    val outputDocFile = (Compile / target).value / "tunables.md"
    // parse xsd file
    val dafExtXml = scala.xml.XML.loadFile(dafExtFile)
    // extract tunables information
    val tunablesElements =
      (dafExtXml \ "element").filter(_ \@ "name" == "tunables") \\ "all" \ "element"
    // build documentation
    val documentationTuple = tunablesElements.map { ele =>
      val subtitle = ele \@ "name"
      val documentation =
        (ele \ "annotation" \ "documentation").text.trim.split("\n").map(_.trim).mkString("\n")
      val default = ele \@ "default"
      (subtitle, documentation, default)
    }
    val (deprecated, nonDeprecated) = documentationTuple.partition { t =>
      t._2.startsWith("Deprecated")
    }
    if (nonDeprecated.isEmpty)
      throw new MessageOnlyException(
        "tunables document generation failed as non-deprecated elements list is empty"
      )

    val nonDeprecatedDefs = nonDeprecated.map { case (sub, desc, default) =>
      s"""
       |#### $sub
       |$desc
       |
       |default: $default
       |""".stripMargin
    }
    val deprecatedList = deprecated.map(_._1)
    val documentationPage =
      s"""|---
      |layout: page
      |title: Tunables
      |group: nav-right
      |---
      |<!--
      |{% comment %}
      |Licensed to the Apache Software Foundation (ASF) under one or more
      |contributor license agreements.  See the NOTICE file distributed with
      |this work for additional information regarding copyright ownership.
      |The ASF licenses this file to you under the Apache License, Version 2.0
      |(the "License"); you may not use this file except in compliance with
      |the License.  You may obtain a copy of the License at
      |
      |http://www.apache.org/licenses/LICENSE-2.0
      |
      |Unless required by applicable law or agreed to in writing, software
      |distributed under the License is distributed on an "AS IS" BASIS,
      |WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
      |See the License for the specific language governing permissions and
      |limitations under the License.
      |{% endcomment %}
      |-->
      |<!--
      |{% comment %}
      |This file is generated using ``sbt genTunablesDoc``. Update that task in Daffodil to update this file.
      |{% endcomment %}
      |-->
      |
      |Daffodil provides tunables as a way to change its behavior.
      |Tunables are set by way of the ``tunables`` element in [config files](/configuration)
      |or from the [cli](/cli) via the ``-T`` option.
      |
      |#### Config Example
      | ``` xml
      | <daf:dfdlConfig
      |	xmlns:daf="urn:ogf:dfdl:2013:imp:daffodil.apache.org:2018:ext">
      |    <daf:tunables>
      |      <daf:suppressSchemaDefinitionWarnings>
      |        encodingErrorPolicyError
      |      </daf:suppressSchemaDefinitionWarnings>
      |    </daf:tunables>
      |</daf:dfdlConfig>
      | ```
      |
      | The config file can then be passed into daffodil subcommands via the ``-c|--config`` options.
      |
      |#### CLI Example
      | ``` bash
      | daffodil parse -s schema.xsd -TsuppressSchemaDefinitionWarnings="encodingErrorPolicyError" data.bin
      | ```
      |
      |
      |### Definitions
      |${nonDeprecatedDefs.mkString("\n")}
      |
      |### Deprecated
      |${deprecatedList.mkString("- ", "\n- ", "")}
      |""".stripMargin
    IO.write(outputDocFile, s"$documentationPage")
    stream.log.info(s"generated tunables documentation at: $outputDocFile")
    Seq(outputDocFile)
  }
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
  }
)
