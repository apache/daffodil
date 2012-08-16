import sbt._
import Keys._
import de.johoop.jacoco4sbt._
import JacocoPlugin._

object DaffodilBuild extends Build {

	var s = Defaults.defaultSettings
	lazy val nopub = Seq(publish := {}, publishLocal := {})

	// daffodil projects
	lazy val root    = Project(id = "daffodil", base = file("."), settings = s ++ nopub)
	                           .configs(DebugTest)
	                           .aggregate(propgen, lib, core, test)

	lazy val propgen = Project(id = "daffodil-propgen", base = file("daffodil-propgen"), settings = s)
	                           .configs(DebugTest)

	lazy val lib     = Project(id = "daffodil-lib", base = file("daffodil-lib"), settings = s ++ propgenSettings)
	                           .configs(DebugTest)
	                           .dependsOn(propgen)

	lazy val core    = Project(id = "daffodil-core", base = file("daffodil-core"), settings = s)
	                           .configs(DebugTest)
	                           .dependsOn(lib)

	lazy val test    = Project(id = "daffodil-test", base = file("daffodil-test"), settings = s ++ nopub)
	                           .configs(DebugTest)
	                           .dependsOn(core)

	val propertyGenerator = TaskKey[Seq[File]]("gen-props", "Generate properties scala source")
	lazy val propgenSettings = Seq(
		sourceGenerators in Compile <+= (propertyGenerator in Compile),
		propertyGenerator in Compile <<=
			(sourceManaged in Compile, dependencyClasspath in Runtime in propgen) map {
				(outdir, cp) => runPropertyGenerator(outdir, cp.files)
			}
	)

	def runPropertyGenerator(outdir : File, cp: Seq[File]): Seq[File] = {
		val mainClass = "daffodil.propGen.PropertyGenerator"
		val out = new java.io.ByteArrayOutputStream()
		val ret = new Fork.ForkScala(mainClass).fork(None, Nil, cp, Seq(outdir.toString), None, false, CustomOutput(out)).exitValue()
		if (ret != 0) {
			sys.error("Failed to generate code")
		}
		val in = new java.io.InputStreamReader(new java.io.ByteArrayInputStream(out.toByteArray))
		val bin = new java.io.BufferedReader(in)
		val iterator = Iterator.continually(bin.readLine()).takeWhile(_ != null)
		val files = iterator.map(f => new File(f)).toList
		files	
	}

	// modify the managed source directories so that any generated code can be more easily included in IDE's
	s ++= Seq(sourceManaged <<= baseDirectory(_ / "src_managed"))

	// creates 'sbt debug:*' tasks, using src/test/scala-debug as the source directory
	lazy val DebugTest = config("debug") extend(Test)
	lazy val debugSettings: Seq[Setting[_]] = inConfig(DebugTest)(Defaults.testSettings ++ Seq(
		sourceDirectory <<= baseDirectory(_ / "src" / "test"),
		scalaSource <<= sourceDirectory(_ / "scala-debug")
	))
	s ++= Seq(debugSettings : _*)
	
	// creates 'sbt debug' task, which is essentially an alias for 'sbt debug:test'
	lazy val debugTask = TaskKey[Unit]("debug", "Executes all debug tests")
	lazy val debugTaskSettings = debugTask <<= (executeTests in DebugTest, streams in DebugTest) map {
		(results, s) => Tests.showResults(s.log, results)
	}
	s ++= Seq(debugTaskSettings)
	
	// jacoco configuration
	s ++= Seq(jacoco.settings : _*)
}
