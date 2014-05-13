import sbt._
import Keys._
import de.johoop.jacoco4sbt._
import JacocoPlugin._
import com.typesafe.sbt.SbtStartScript

object DaffodilBuild extends Build {

  var s = Defaults.defaultSettings
  lazy val nopub = Seq(publish := {}, publishLocal := {})

  // sbt uses reflection to find suprojects. we need to also add the projects
  // in daffodil-extra, which can't be detected via reflection
  override def projects: Seq[Project] = super.projects ++ extraProjects

  // daffodil projects
  lazy val root = {
    val r = Project(id = "daffodil", base = file("."), settings = s ++ nopub)
                    .configs(DebugTest)
                    .configs(NewTest)
                    .configs(CliTest)
                    .aggregate(propgen, lib, io, core, runtime1, tdml, testIBM1, cli, test, examples)
    extraProjects.foldLeft(r) { (r, p) => r.aggregate(p) }
  }

  lazy val propgen = Project(id = "daffodil-propgen", base = file("daffodil-propgen"), settings = s ++ nopub)
                             .configs(DebugTest)
                             .configs(NewTest)

  lazy val lib     = Project(id = "daffodil-lib", base = file("daffodil-lib"), settings = s ++ propgenSettings)
                             .configs(DebugTest)
                             .configs(NewTest)   

  lazy val io    = Project(id = "daffodil-io", base = file("daffodil-io"), settings = s)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(lib) 

  lazy val core    = Project(id = "daffodil-core", base = file("daffodil-core"), settings = s)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(io) 

  lazy val runtime1    = Project(id = "daffodil-runtime1", base = file("daffodil-runtime1"), settings = s)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(core % "compile->compile;test->test") // test in core has utilities that test in runtime1 needs.

                             
  lazy val tdml    = Project(id = "daffodil-tdml", base = file("daffodil-tdml"), settings = s)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(runtime1)
                             
  lazy val cli    = Project(id = "daffodil-cli", base = file("daffodil-cli"), settings = s ++ startScriptSettings ++ stageTaskSettings)
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
  //TODO: find a way to clean this up and reduce repetition
  lazy val testTask = Keys.test in CliTest
  lazy val testOnlyTask = Keys.testOnly in CliTest
  lazy val testQuickTask = Keys.testQuick in CliTest
  
  lazy val testTaskNew = Keys.test in NewTest
  lazy val testOnlyTaskNew = Keys.testOnly in NewTest
  lazy val testQuickTaskNew = Keys.testQuick in NewTest
  
  lazy val testTaskDebug = Keys.test in DebugTest
  lazy val testOnlyTaskDebug = Keys.testOnly in DebugTest
  lazy val testQuickTaskDebug = Keys.testQuick in DebugTest

  lazy val stageTask = SbtStartScript.stage in Compile //  in cli

  lazy val stageTaskSettings = Seq(
    //cli test tasks
    (testTask <<= testTask.dependsOn(stageTask)),
    (testOnlyTask <<= testOnlyTask.dependsOn(stageTask)),
    (testQuickTask <<= testQuickTask.dependsOn(stageTask)),
    //new test tasks
    (testTaskNew <<= testTaskNew.dependsOn(stageTask)),
    (testOnlyTaskNew <<= testOnlyTaskNew.dependsOn(stageTask)),
    (testQuickTaskNew <<= testQuickTaskNew.dependsOn(stageTask)),
    //debug test tasks
    (testTaskDebug <<= testTaskDebug.dependsOn(stageTask)),
    (testOnlyTaskDebug <<= testOnlyTaskDebug.dependsOn(stageTask)),
    (testQuickTaskDebug <<= testQuickTaskDebug.dependsOn(stageTask)),
    //cli, new, and debug tasks
    (debugTask <<= debugTask.dependsOn(stageTask)),
    (newTask <<= newTask.dependsOn(stageTask)),
    (cliTask <<= cliTask.dependsOn(stageTask))
  )

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

  // modify the managed source directories so that any generated code can be more easily included in IDE's
  s ++= Seq(sourceManaged <<= baseDirectory(_ / "src_managed"))

  // creates 'sbt debug:*' tasks, using src/test/scala-debug as the source directory
  lazy val DebugTest = config("debug") extend(Runtime)
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
  lazy val NewTest = config("new") extend(Runtime)
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
  lazy val CliTest = config("cli") extend(Runtime)
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

  // jacoco configuration
  s ++= Seq(jacoco.settings : _*)

  // start-script configuration
  lazy val startScriptSettings = Seq(SbtStartScript.startScriptForJarSettings : _*) ++
                                 Seq(mainClass in Compile := Some("edu.illinois.ncsa.daffodil.Main"))


  // test report plugin configuration
  lazy val testReportSettings = testListeners <+= (crossTarget) map {
    ct => new com.dadrox.sbt.test.reports.Xml(ct.getPath)
  }
  s ++= Seq(testReportSettings)


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
        val branch = exec("git rev-parse --abbrev-ref HEAD")
        assert(branch.length == 1)
        val VersionBranchRegex = """^(\d+\.\d+\.\d+)$""".r
        branch(0) match {
          case "HEAD" => {
            // not on the tip of a branch
            "0.0.0-SNAPSHOT"
          }
          case VersionBranchRegex(versionBranch) => {
            // we are developing on a version branch, create a snapshot
            versionBranch + "-SNAPSHOT"
          }
          case _ => {
            // not on a version branch (e.g. a review branch), try to figure
            // out the tracking branch
            val trackingBranch = exec("git for-each-ref --format=%(upstream:short) refs/heads/" + branch(0))
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
}
