/* Copyright (c) 2017 Tresys Technology, LLC. All rights reserved.
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
    //
    val jars = jarsDir.listFiles().filterNot { _.getName == "org.scala-lang" }.
      flatMap { _.listFiles().flatMap { _.listFiles() } }
    val srcs = srcsDir.listFiles().flatMap { _.listFiles().flatMap { _.listFiles() } }
    val docs = docsDir.listFiles().flatMap { _.listFiles().flatMap { _.listFiles() } }

    val triples = (bundles ++ jars).map { jar =>
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
