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

import scala.xml._

/**
 * Run this app if you change dependencies on libraries in the build.sbt file.
 * First do a 'sbt update-classifiers compile'. Then run this.
 * <p>
 * This will edit all the eclipse-project/.classpath files to reference the new
 * versions, setting up the source and javadoc links as well for eclipse.
 *
 * Then you have to refresh all the daffodil eclipse projects in eclipse, and do a clean
 * and build.
 */

object UpdateDaffodilClasspaths extends UpdateEclipseClasspaths with App {

  override lazy val libRoot = new java.io.File(dafHome + "/lib_managed")
  override lazy val baseFile = new java.io.File(dafHome)

  def main(): Int = {

    val epl = new java.io.File(dafHome + "/eclipse-projects").listFiles()
    val classPathFiles = epl.flatMap { _.listFiles().filter { _.getName == ".classpath" } }

    classPathFiles.foreach {
      updateOneClasspathFile(_)
    }

    0
  }

  main()
}

trait UpdateEclipseClasspaths {

  lazy val dafHome = {
    //
    // You must have DAFFODIL_HOME defined in your ~/.bash_aliases file
    // and if running this from eclipse, you must invoke eclipse in a manner
    // that has this shell env var defined.
    //
    // Surprisingly to me, standard login does not run the .bash_aliases file
    // so the definitions in it are NOT by default part of the environment for
    // programs that are launched without starting a shell first.
    //
    // I modified my ~/.profile to also load .bash_aliases to fix this.
    //
    val s = System.getenv("DAFFODIL_HOME")
    assert(s ne null)
    s
  }

  def baseFile: java.io.File
  lazy val baseURI = baseFile.toURI

  def libRoot: java.io.File

  lazy val updatedLibChildren: Seq[Node] = {
    assert(libRoot.exists())
    val bundlesDir = libRoot.listFiles().find { _.getName == "bundles" }.get
    val jarsDir = libRoot.listFiles().find { _.getName == "jars" }.get
    val srcsDir = libRoot.listFiles().find { _.getName == "srcs" }.get
    val docsDir = libRoot.listFiles().find { _.getName == "docs" }.get
    val bundles = bundlesDir.listFiles().flatMap { _.listFiles().flatMap { _.listFiles() } }
    //
    // Exception for org.scala-lang as those are on the classpath because these
    // projects are "scala" projects. If we don't exclude this, then we get double-entry
    // complaints about the classpath

    // Also exception for com.typesafe.genjavadoc which is needed by tools
    // and as a "compiler plugin" sbt puts a dependency on this into lib_managed.
    // This is tool only used when we generate javadoc.
    //
    val jars = jarsDir.listFiles().
      filterNot { _.getName == "org.scala-lang" }. // remove scala-lang
      filterNot { _.getName == "com.typesafe.genjavadoc" }. // remove genjavadoc
      flatMap { _.listFiles().flatMap { _.listFiles() } }

    val srcs = srcsDir.listFiles().flatMap { _.listFiles().flatMap { _.listFiles() } }
    val docs = docsDir.listFiles().flatMap { _.listFiles().flatMap { _.listFiles() } }

    //
    // Remove duplicates between the bundles directory and jars directory
    // (Not sure why sbt populates both, but many (all?) things found under
    // the bundles directory are also found under jars.
    //
    // Prefer the jars directory if things are found in two places.
    //
    val jarsNames = jars.map { _.getName() }
    val bundlesWithoutDups = bundles.filterNot { f => jarsNames.contains(f.getName()) }
    val allJars = bundlesWithoutDups ++ jars
    val triples = allJars.map { jar =>
      val src = srcs.find(_.getName.startsWith(jar.getName.split(".jar")(0)))
      val doc = docs.find(_.getName.startsWith(jar.getName.split(".jar")(0)))
      (jar, src, doc)
    }
    // triples.foreach { println(_) }

    val xmlNodes = triples map {
      case (jar, optSrc, optDoc) =>
        val jarStr = {
          val rel = baseURI.relativize(jar.toURI)
          rel.toString
        }
        val optSrcString = optSrc.map { src => baseURI.relativize(src.toURI).toString }
        val optDocString = optDoc.map { doc => baseURI.relativize(doc.toURI).toString }
        val optSrcAttr = optSrcString.map { srcString => new UnprefixedAttribute("sourcepath", srcString, Null) }
        val cpe =
          <classpathentry exported="true" kind="lib" path={ jarStr }>
            {
              optDocString.map { docStr =>
                <attributes>
                  <attribute name="javadoc_location" value={ "jar:file:" + docStr + "!/" }/>
                </attributes>
              }.toSeq
            }
          </classpathentry>
        val cpeWithSrc = if (optSrcAttr.isDefined) cpe % optSrcAttr.get else cpe
        cpeWithSrc
    }
    xmlNodes.toSeq
  }

  def updateOneClasspathFile(cpf: java.io.File) {
    val cpNode = scala.xml.XML.loadFile(cpf)
    val cpes = (cpNode \\ "classpathentry")
    val nonLibChildren = cpes.filterNot { e => (e \\ "@kind").text == "lib" }
    val newEntries = nonLibChildren ++
      new Comment("""
***********
*********** Entries below this comment are maintained using the UpdateEclipseClasspaths
*********** Utility and should not be modified by hand or using the Eclipse
*********** BuildPath... GUI dialog.
***********
""") ++
      updatedLibChildren
    val newCP =
      <classpath>
        <!-- This file is updated by the UpdateEclipseClasspath app. -->
        { newEntries }
      </classpath>
    scala.xml.XML.save(cpf.toString(), newCP)
  }
}
