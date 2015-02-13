/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

import sbt._
import Keys._
import scala.language.existentials
import com.typesafe.sbt.SbtNativePackager._
import NativePackagerKeys._
import com.typesafe.sbt.SbtLicenseReport.autoImportImpl._
import com.typesafe.sbt.license.LicenseCategory
import com.typesafe.sbt.license.LicenseInfo
import com.typesafe.sbt.license.DepModuleInfo
import com.typesafe.sbt.license.Html
import com.typesafe.sbt.pgp.PgpSettings._

object DaffodilBuild extends Build {

  var s = Defaults.defaultSettings
  lazy val nopub = Seq(publish := {}, publishLocal := {}, packagedArtifacts := Map.empty )

  var cliOnlySettings = packageArchetype.java_application

  // sbt uses reflection to find suprojects. we need to also add the projects
  // in daffodil-extra, which can't be detected via reflection
  override def projects: Seq[Project] = super.projects ++ extraProjects

  // daffodil projects
  lazy val root = {
    val r = Project(id = "daffodil", base = file("."), settings = s ++ nopub)
                    .configs(DebugTest)
                    .configs(NewTest)
                    .configs(CliTest)
                    .aggregate(propgen, lib, io, runtime1, core, tdml, testIBM1, cli, test, examples, japi, sapi)
    extraProjects.foldLeft(r) { (r, p) => r.aggregate(p) }
  }

  lazy val propgen = Project(id = "daffodil-propgen", base = file("daffodil-propgen"), settings = s ++ nopub)
                             .configs(DebugTest)
                             .configs(NewTest)

  lazy val lib     = Project(id = "daffodil-lib", base = file("daffodil-lib"), settings = s ++ propgenSettings ++ schemasgenSettings ++ managedgenSettings)
                             .configs(DebugTest)
                             .configs(NewTest)

  lazy val io    = Project(id = "daffodil-io", base = file("daffodil-io"), settings = s)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(lib)

  lazy val runtime1    = Project(id = "daffodil-runtime1", base = file("daffodil-runtime1"), settings = s)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(io)

  lazy val runtime1Unparser    = Project(id = "daffodil-runtime1-unparser", base = file("daffodil-runtime1-unparser"), settings = s)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(runtime1)

  lazy val core    = Project(id = "daffodil-core", base = file("daffodil-core"), settings = s)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(runtime1Unparser)

  lazy val japi    = Project(id = "daffodil-japi", base = file("daffodil-japi"), settings = s ++ genJavaDocSettings)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(core)

  lazy val sapi    = Project(id = "daffodil-sapi", base = file("daffodil-sapi"), settings = s ++ sapiSettings)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(core)

  lazy val tdml    = Project(id = "daffodil-tdml", base = file("daffodil-tdml"), settings = s)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(core)

  lazy val cli    = Project(id = "daffodil-cli", base = file("daffodil-cli"), settings = s ++ cliOnlySettings)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .configs(CliTest)
                             .dependsOn(tdml)

  lazy val test    = Project(id = "daffodil-test", base = file("daffodil-test"), settings = s ++ nopub)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(tdml)
                             .dependsOn(core % "test->test") // need test utilities in core's test dirs

  lazy val examples    = Project(id = "daffodil-examples", base = file("daffodil-examples"), settings = s ++ nopub)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(tdml)

  lazy val testIBM1    = Project(id = "daffodil-test-ibm1", base = file("daffodil-test-ibm1"), settings = s ++ nopub)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(tdml)

  // any directories in daffodil-extra are treated as subprojects. this is
  // useful for symlinking tests that should not be committed to the git repo
  // but still be compiled/tested as part of daffodil
  lazy val extrasDir = new File("daffodil-extra")
  lazy val extraProjects = extrasDir.listFiles.filter { _.isDirectory }.map { dir =>
    Project(id = dir.name, base = dir, settings = s ++ nopub)
           .configs(DebugTest)
           .configs(NewTest)
           .dependsOn(tdml)
  }


  //set up 'sbt stage' as a dependency
  lazy val cliTestTask = Keys.test in CliTest
  lazy val cliTestOnlyTask = Keys.testOnly in CliTest
  lazy val cliTestQuickTask = Keys.testQuick in CliTest
  lazy val stageTask = stage in Compile

  lazy val stageTaskSettings = Seq(
    (cliTestTask <<= cliTestTask.dependsOn(stageTask)),
    (cliTestOnlyTask <<= cliTestOnlyTask.dependsOn(stageTask)),
    (cliTestQuickTask <<= cliTestQuickTask.dependsOn(stageTask)),
    (cliTask <<= cliTask.dependsOn(stageTask))
  )
  cliOnlySettings ++= stageTaskSettings

  cliOnlySettings ++= Seq(exportJars in Test := true)

  val managedGenerator = TaskKey[Unit]("gen-managed", "Generate managed sources and resources")
  lazy val managedgenSettings = managedGenerator <<= Seq(propertyGenerator in Compile, schemasGenerator in Compile).dependOn


  val propertyGenerator = TaskKey[Seq[File]]("gen-props", "Generate properties scala source")
  lazy val propgenSettings = Seq(
    sourceGenerators in Compile <+= (propertyGenerator in Compile),
    propertyGenerator in Compile <<=
      (sourceManaged in Compile, dependencyClasspath in Runtime in propgen, sources in Compile in propgen, resources in Compile in propgen, streams in propgen) map {
        (outdir, cp, inSrc, inRSrc, stream) => {
          // FileFunction.cached will only run the property generator if any
          // source or resources in propgen subproject changed
          val filesToWatch = (inSrc ++ inRSrc).toSet
          val cachedFun = FileFunction.cached(stream.cacheDirectory / "propgen", FilesInfo.lastModified, FilesInfo.exists) {
            (in: Set[File]) => runPropertyGenerator(outdir, cp.files, stream.log)
          }
          cachedFun(filesToWatch).toSeq
        }
      }
  )

  def runPropertyGenerator(outdir: File, cp: Seq[File], log: Logger): Set[File] = {
    val mainClass = "edu.illinois.ncsa.daffodil.propGen.PropertyGenerator"
    val out = new java.io.ByteArrayOutputStream()
    val forkOpts = new ForkOptions(None, Some(CustomOutput(out)), cp, None, Nil, false)
    val ret = new Fork("java", Some(mainClass)).fork(forkOpts, Seq(outdir.toString)).exitValue()
    if (ret != 0) {
      sys.error("Failed to generate code")
    }
    val in = new java.io.InputStreamReader(new java.io.ByteArrayInputStream(out.toByteArray))
    val bin = new java.io.BufferedReader(in)
    val iterator = Iterator.continually(bin.readLine()).takeWhile(_ != null)
    val files = iterator.map { f =>
      log.info("Generated %s".format(f))
      new File(f)
    }.toSet
    files
  }

  val schemasGenerator = TaskKey[Seq[File]]("gen-schemas", "Generated DFDL schemas")
  lazy val schemasgenSettings = Seq(
    resourceGenerators in Compile <+= (schemasGenerator in Compile),
    schemasGenerator in Compile <<=
      (resourceManaged in Compile, resources in Compile in propgen, streams in propgen) map {
        (outdir, inRSrc, stream) => {
          // FileFunction.cached will only run the schema generator if any
          // resources in propgen subproject changed
          val filesToWatch = inRSrc.toSet
          val cachedFun = FileFunction.cached(stream.cacheDirectory / "schemasgen", FilesInfo.lastModified, FilesInfo.exists) {
            (in: Set[File]) => copyResources(outdir, inRSrc.toSet, stream.log)
          }
          cachedFun(filesToWatch).toSeq
        }
      }
  )

  def copyResources(outdir: File, inRSrc: Set[File], log: Logger): Set[File] = {
    val dfdlSchemas = inRSrc.filter { f => f.getName.matches("DFDL_part.*\\.xsd") }
    val managed_resources = dfdlSchemas.map { in =>
      val out = outdir / "xsd" / in.getName
      IO.copyFile(in, out)
      log.info("Generated %s".format(out))
      out
    }
    managed_resources
  }

  // modify the managed source and resource directories so that any generated code can be more easily included in IDE's
  s ++= Seq(sourceManaged <<= baseDirectory(_ / "src_managed"))

  s ++= Seq(resourceManaged <<= baseDirectory(_ / "resource_managed"))

  // modify the managed libraries directory to be lib/jars/ instead of lib,
  // allowing us to manage sources/docs in lib/srcs and lib/docs without them
  // being included as a dependency
  s ++= Seq(unmanagedBase <<= baseDirectory(_ / "lib" / "jars"))


  // creates 'sbt debug:*' tasks, using src/test/scala-debug as the source directory
  lazy val DebugTest = config("debug") extend(Test)
  lazy val debugSettings: Seq[Setting[_]] = inConfig(DebugTest)(Defaults.testSettings ++ Seq(
    sourceDirectory <<= baseDirectory(_ / "src" / "test"),
    scalaSource <<= sourceDirectory(_ / "scala-debug"),
    javaSource <<= sourceDirectory(_ / "java-debug"),
    exportJars := false,
    publishArtifact := false
  ))
  s ++= Seq(debugSettings : _*)

  // creates 'sbt debug' task, which is essentially an alias for 'sbt debug:test'
  lazy val debugTask = TaskKey[Unit]("debug", "Executes all debug tests")
  lazy val debugTaskSettings = debugTask <<= (executeTests in DebugTest, streams in DebugTest, resolvedScoped in DebugTest, state in DebugTest) map {
    (results, s, scoped, state) => {
      val display = Project.showContextKey(state)
      Tests.showResults(s.log, results, "No tests to run for " + display(scoped))
    }
  }
  s ++= Seq(debugTaskSettings)

  // creates 'sbt new:*' tasks, using src/test/scala-new as the source directory
  lazy val NewTest = config("new") extend(Test)
  lazy val newSettings: Seq[Setting[_]] = inConfig(NewTest)(Defaults.testSettings ++ Seq(
    sourceDirectory <<= baseDirectory(_ / "src" / "test"),
    scalaSource <<= sourceDirectory(_ / "scala-new"),
    javaSource <<= sourceDirectory(_ / "java-new"),
    exportJars := false,
    publishArtifact := false

  ))
  s ++= Seq(newSettings : _*)

  // creates 'sbt new' task, which is essentially an alias for 'sbt new:test'
  lazy val newTask = TaskKey[Unit]("new", "Executes all new tests")
  lazy val newTaskSettings = newTask <<= (executeTests in NewTest, streams in NewTest, resolvedScoped in NewTest, state in NewTest) map {
    (results, s, scoped, state) => {
      val display = Project.showContextKey(state)
      Tests.showResults(s.log, results, "No tests to run for " + display(scoped))
    }
  }
  s ++= Seq(newTaskSettings)

  // add scala-new as a source test directory for the 'sbt test' commands
  lazy val buildNewWithTestSettings = unmanagedSourceDirectories in Test <++= baseDirectory { base =>
    Seq(base / "src/test/scala-new")
  }
  s ++= Seq(buildNewWithTestSettings)

  // creates 'sbt cli:*' tasks, using src/test/scala-cli as the source directory
  lazy val CliTest = config("cli") extend(Test)
  lazy val cliSettings: Seq[Setting[_]] = inConfig(CliTest)(Defaults.testSettings ++ Seq(
    sourceDirectory <<= baseDirectory(_ / "src" / "test"),
    scalaSource <<= sourceDirectory(_ / "scala-cli"),
    javaSource <<= sourceDirectory(_ / "java-cli"),
    exportJars := false,
    publishArtifact := false

  ))
  s ++= Seq(cliSettings : _*)

  // creates 'sbt cli' task, which is essentially an alias for 'sbt cli:test'
  lazy val cliTask = TaskKey[Unit]("cli", "Executes all CLI tests")
  lazy val cliTaskSettings = cliTask <<= (executeTests in CliTest, streams in CliTest, resolvedScoped in CliTest, state in CliTest) map {
    (results, s, scoped, state) => {
      val display = Project.showContextKey(state)
      Tests.showResults(s.log, results, "No tests to run for " + display(scoped))
    }
  }
  s ++= Seq(cliTaskSettings)

  // license report configuration
  val licenseSettings = Seq(
    licenseReportTitle := "Daffodil_Licenses",  // has an underscore since this is used to create the output file
    licenseConfigurations := Set("compile"),
    licenseSelection := Seq(LicenseCategory("NCSA"), LicenseCategory("ICU")) ++ LicenseCategory.all,
    licenseOverrides := {
      case DepModuleInfo("commons-io", "commons-io", _) => LicenseInfo(LicenseCategory.Apache, "The Apache Software License, Version 2.0", "http://www.apache.org/licenses/LICENSE-2.0.html")
      case DepModuleInfo("net.sf.expectit", "expectit-core", _) => LicenseInfo(LicenseCategory.Apache, "The Apache Software License, Version 2.0", "http://www.apache.org/licenses/LICENSE-2.0.html")
      case DepModuleInfo("xml-resolver", "xml-resolver", _) => LicenseInfo(LicenseCategory.Apache, "The Apache Software License, Version 2.0", "http://www.apache.org/licenses/LICENSE-2.0.html")
      case DepModuleInfo("org.scala-tools.testing", "test-interface", _) => LicenseInfo(LicenseCategory.BSD, "BSD", "https://github.com/sbt/test-interface/blob/master/LICENSE")
      case DepModuleInfo("org.hamcrest", "hamcrest-core", _) => LicenseInfo(LicenseCategory.BSD, "BSD", "https://github.com/hamcrest/JavaHamcrest/blob/master/LICENSE.txt")
    },
    licenseFilter := {
      case LicenseCategory("NCSA", _) => false
      case _ => true
    },
    licenseReportMakeHeader := {
      case Html => Html.header1(licenseReportTitle.value.replace("_", " ")) + "<p>Daffodil is licensed under the <a href='http://opensource.org/licenses/NCSA'>University of Illinois/NCSA Open Source License</a>.</p><p>Below are the libraries that Daffodil depends on and their licenses.<br></p>"
      case l => l.header1(licenseReportTitle.value.replace("_", " "))
    }
  )
  cliOnlySettings ++= licenseSettings

  // native package configuration
  val packageSettings = Seq(
    packageName := "daffodil",
    mappings in Universal ++= createRecursiveMapping(file("daffodil-examples/src/test/resources/edu/illinois/ncsa/daffodil/"), "examples"),
    mappings in Universal += dumpLicenseReport.value / (licenseReportTitle.value + ".html") -> "LICENSES.html",
    mappings in Universal += baseDirectory.value / "README" -> "README",
    mappings in Universal <+= (packageBin in japi in Compile, name in japi in Compile, organization, version) map {
      (bin, n, o, v) => bin -> "lib/%s.%s-%s.jar".format(o, n, v)
    },
    mappings in Universal <+= (packageBin in sapi in Compile, name in sapi in Compile, organization, version) map {
      (bin, n, o, v) => bin -> "lib/%s.%s-%s.jar".format(o, n, v)
    }
  )
  cliOnlySettings ++= packageSettings


  def exec(cmd: String): Seq[String] = {
    val r = java.lang.Runtime.getRuntime()
    val p = r.exec(cmd)
    p.waitFor()
    val ret = p.exitValue()
    if (ret != 0) {
      sys.error("Command failed: " + cmd)
    }
    val is = p.getInputStream
    val res = scala.io.Source.fromInputStream(is).getLines()
    res.toSeq
  }

  // get the version from the latest tag
  s ++= Seq(version := {
    val describe = exec("git describe --long HEAD")
    assert(describe.length == 1)

    val DescribeRegex = """^(.+)-(.+)-(.+)$""".r
    val res = describe(0) match {
      case DescribeRegex(taggedVersion, "0", hash) => {
        // we are on a tag, build a tag release
        val status = exec("git status --porcelain")
        if (status.length > 0) {
          taggedVersion + "-SNAPSHOT"
        } else {
          taggedVersion
        }
      }
      case DescribeRegex(version, _, hash) => {
        // not on a tag

        // get the current branch
        val branch = exec("git rev-parse --symbolic-full-name HEAD")
        assert(branch.length == 1)
        val VersionBranchRegex = """^refs/heads/(\d+\.\d+\.\d+)$""".r
        branch(0) match {
          case "HEAD" => {
            // not on the tip of a branch
            "0.0.0-SNAPSHOT"
          }
          case VersionBranchRegex(versionBranch) => {
            // we are developing on a version branch, create a snapshot
            versionBranch + "-SNAPSHOT"
          }
          case branch => {
            // not on a version branch (e.g. a review branch), try to figure
            // out the tracking branch
            val trackingBranch = exec("git for-each-ref --format=%(upstream:short) " + branch)
            assert(trackingBranch.length == 1)
            val TrackingBranchRegex = """^[^/]+/(\d+\.\d+\.\d+)$""".r
            trackingBranch(0) match {
              case TrackingBranchRegex(trackingVersion) => {
                trackingVersion + "-SNAPSHOT"
              }
              case _ => {
                // no idea what the version is, set it to a default
                "0.0.0-SNAPSHOT"
              }
            }
          }
        }
      }
    }
    res
  })

  def gitShortHash(): String = {
    val hash = exec("git rev-parse --short HEAD")
    assert(hash.length == 1)
    hash(0)
  }


  // update the manifest version to include the git hash
  lazy val manifestVersion = packageOptions in (Compile, packageBin) <++= version map { v => {
    val version = "%s-%s".format(v, gitShortHash)
    Seq(
      Package.ManifestAttributes(java.util.jar.Attributes.Name.IMPLEMENTATION_VERSION -> version),
      Package.ManifestAttributes(java.util.jar.Attributes.Name.SPECIFICATION_VERSION -> version)
    )
  }}
  s ++= manifestVersion

  // by default, sbt-pgp tries to read ~/.gnupg for keys. We need to force it
  // to be .sbt, which makes it easier to pass the keys around without having
  // to import into gnupg keyrings
  lazy val pgpSettings = Seq(
    pgpPublicRing := file(System.getProperty("user.home")) / ".sbt" / "gpg" / "daffodil" / "pubring.asc",
    pgpSecretRing := file(System.getProperty("user.home")) / ".sbt" / "gpg" / "daffodil" / "secring.asc"
  )
  s ++= pgpSettings


  lazy val GenJavaDoc = config("genjavadoc") extend Compile
  lazy val genJavaDocSettings = inConfig(GenJavaDoc)(Defaults.configSettings) ++ Seq(
    libraryDependencies += compilerPlugin("com.typesafe.genjavadoc" %% "genjavadoc-plugin" % "0.8" cross CrossVersion.full),
    scalacOptions <+= target map (t => "-P:genjavadoc:out=" + (t / "java")),
    packageDoc in Compile <<= packageDoc in GenJavaDoc,
    sources in GenJavaDoc <<= (target, compile in Compile, sources in Compile) map ((t, c, s) => (t / "java" ** "*.java").get.filterNot(f => f.toString.contains('$') || f.toString.contains("packageprivate")) ++ s.filter(_.getName.endsWith(".java"))),
    artifactName in packageDoc in GenJavaDoc := ((sv, mod, art) => "" + mod.name + "_" + sv.binary + "-" + mod.revision + "-javadoc.jar"),
    javacOptions in GenJavaDoc <<= (version) map ((v) => Seq("-quiet", "-windowtitle", "Daffodil-" + v + " Java API", "-doctitle", "<h1>Daffodil-" + v + " Java API</h1>"))
  )

  lazy val sapiSettings = Seq(
    scalacOptions in (Compile, doc) <<= (version, baseDirectory) map ((v,b) => Seq("-doc-title", "Daffodil-" + v + " Scala API", "-doc-root-content", b + "/root-doc.txt"))
  )

  def createRecursiveMapping(dir: File, newDir: String): Seq[(File,String)] = {
    // this recursively gathers all files in the daffodil-examples resources
    // directory and creates a mapping to the relative location in the
    // 'examples' directory which is packaged in zip/tars
    val allFiles = dir.***
    val basename = dir.getName
    val mappings = allFiles.pair { f: File =>
      val relative = f.relativeTo(dir.getParentFile).get.toString
      val exRelative = relative.replaceFirst("^" + basename, newDir)
      Some(exRelative)
    }
    mappings
  }

}
