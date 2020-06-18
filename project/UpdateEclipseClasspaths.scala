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
import java.io.PrintStream

import scala.language.reflectiveCalls

/**
 * After 'sbt eclipse' creates the projects, you still have to update every .classpath file
 * so that all but daffodil-macro-lib have a dependency on daffodil-macro-lib.
 */

object UpdateEclipseClasspaths extends App {

  lazy val libRoot = new java.io.File(dafHome + "/lib_managed")
  lazy val baseFile = new java.io.File(dafHome)

  def main(): Int = {

    val epl = new java.io.File(dafHome).listFiles()
    val classPathFiles = epl.flatMap { f =>
      if (f.isDirectory())
        f.listFiles().filter { _.getName == ".classpath" }
      else Nil
    }.filterNot { _.toString.contains("daffodil-macro-lib") }

    classPathFiles.foreach { f =>
      println("updating " + f.toString)
      updateOneClasspathFile(f)
    }

    0
  }

  main()

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
    assert(s ne null, "$DAFFODIL_HOME undefined")
    s
  }

  lazy val pp = new scala.xml.PrettyPrinter(3000, 2)

  lazy val baseURI = baseFile.toURI

  def updateOneClasspathFile(cpf: java.io.File): Unit = {
    val cpNode = scala.xml.XML.loadFile(cpf)
    /*
     * There is an issue with the XML loader that reverses the order of the attributes
     * when loaded. So we load it again to get the attributes back in the right order
     */
    val fixedCpNode = XML.loadString(pp.format(cpNode))
    val cpes = (fixedCpNode \\ "classpathentry")
    val newEntries = cpes :+
      <classpathentry combineaccessrules="false" kind="src" path="/daffodil-macro-lib"/>
    val newCP =
      <classpath>
        <!-- This file is updated by the UpdateEclipseClasspath app. -->
        { newEntries }
      </classpath>
    writeXMLFile(newCP, cpf.toString)
  }

  def writeXML(xml: Node, out: { def print(s: String): Unit } = System.out): Unit = {
    val formattedSpec = pp.format(xml)
    out.print("<?xml version='1.0' encoding='UTF-8'?>\n")
    out.print("\n")
    out.print(formattedSpec)
    out.print("\n")
  }

  def writeXMLFile(xml: Node, outputFilename: String): Unit = {
    val f = new java.io.File(outputFilename)
    f.getParentFile().mkdirs()
    val ps = new PrintStream(f)
    writeXML(xml, ps)
    ps.close()
  }
}
