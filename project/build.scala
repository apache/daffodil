import sbt._
import Keys._
import de.johoop.jacoco4sbt._
import JacocoPlugin._
import com.typesafe.startscript.StartScriptPlugin

object DaffodilBuild extends Build {

	var s = Defaults.defaultSettings
	lazy val nopub = Seq(publish := {}, publishLocal := {})

	// daffodil projects
	lazy val root    = Project(id = "daffodil", base = file("."), settings = s ++ nopub)
	                           .aggregate(propgen, lib, core, test)

	lazy val propgen = Project(id = "daffodil-propgen", base = file("daffodil-propgen"), settings = s ++ nopub)

	lazy val lib     = Project(id = "daffodil-lib", base = file("daffodil-lib"), settings = s ++ propgenSettings)

	lazy val core    = Project(id = "daffodil-core", base = file("daffodil-core"), settings = s ++ startScriptSettings)
	                           .dependsOn(lib)

	lazy val test    = Project(id = "daffodil-test", base = file("daffodil-test"), settings = s ++ nopub)
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


	// function for creating a new type of test, creates the 'sbt name' and
	// 'sbt name:*' commands which looks in src/test/scala-X for source files
	def createTestTask(name : String, buildWithTest : Boolean) : Seq[Setting[_]] = {
		// creates 'sbt name:*' tasks, using src/test/scala-name as the source directory
		lazy val NewTest = config(name) extend(Test)
		var newTestSettings: Seq[Setting[_]] = inConfig(NewTest)(Defaults.testSettings ++ Seq(
			sourceDirectory <<= baseDirectory(_ / "src" / "test"),
			scalaSource <<= sourceDirectory(_ / "scala-%s".format(name))
		))
		
		// creates 'sbt name' task, which is essentially an alias for 'sbt name:test'
		lazy val newTestTask = TaskKey[Unit](name, "Executes all %s tests".format(name))
		lazy val newTestTaskSettings = newTestTask <<= (executeTests in NewTest, streams in NewTest) map {
			(results, s) => Tests.showResults(s.log, results)
		}
		newTestSettings ++= Seq(newTestTaskSettings)

		if (buildWithTest) {
			// add scala-new as a source test directory for the 'sbt test' commands
			lazy val buildWithTestSettings = unmanagedSourceDirectories in Test <++= baseDirectory { base =>
				Seq(base / "src/test/scala-%s".format(name))
			}
			newTestSettings ++= Seq(buildWithTestSettings)
		}

		newTestSettings
	}

	// add 'sbt debug' and 'sbt debug:*' commands
	s ++= createTestTask("debug", false)
	
	// add 'sbt new' and 'sbt new:*' commands
	s ++= createTestTask("new", true)


	// jacoco configuration
	s ++= Seq(jacoco.settings : _*)

	// start-script configuration	
	lazy val startScriptSettings = Seq(StartScriptPlugin.startScriptForJarSettings : _*) ++
	                               Seq(mainClass in Compile := Some("daffodil.Main"))
}
