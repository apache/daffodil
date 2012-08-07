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
	                           .aggregate(lib, core, test)

	lazy val lib     = Project(id = "daffodil-lib", base = file("daffodil-lib"), settings = s)
	                           .configs(DebugTest)

	lazy val core    = Project(id = "daffodil-core", base = file("daffodil-core"), settings = s)
	                           .configs(DebugTest)
	                           .dependsOn(lib)

	lazy val test    = Project(id = "daffodil-test", base = file("daffodil-test"), settings = s ++ nopub)
	                           .configs(DebugTest)
	                           .dependsOn(core)

	lazy val propgen = Project(id = "daffodil-propgen", base = file("daffodil-propgen"), settings = s)

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
