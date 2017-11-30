
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

import java.io.File
import scala.io.Source

/**
 * Counts Lines-of-Code in Daffodil, and associated DFDL schemas and demo code.
 *
 * Excludes TDML files that are mostly just big chunks of data.
 * Excludes data files.
 * Includes TDML and Scala and Java and XSD and SBT files.
 */
object LineCounter extends App {

  /**
   * What file extensions to count.
   */
  val fileSuffixesToInclude = List(".scala", ".java", ".tdml", ".xsd") // not .xml as some of those are data files that are giant.

  /**
   * Where is this freakin' sandbox anyway....edit to point to yours.
   */
  val root = "/home/mbeckerle/dataiti/git/"

  /**
   * What modules within the sandbox to count
   */
  val srcModulesToInclude = List("daffodil-core", "daffodil-runtime1", "daffodil-runtime1-unparser", "daffodil-lib", "daffodil-propgen", "daffodil-tdml", "daffodil-io", "daffodil-cli", "daffodil-japi", "daffodil-sapi")
  val unitTestModulesToInclude = srcModulesToInclude

  def top() = {

    val counters = List(
      new LineCounterCombiner("daffodil source", List(
        new LineCounter("daffodil source", root + "daffodil", srcModulesToInclude, List("src/main"),
          filesToExclude = List("GeneratedCode.scala",
            "DFDL_part3_model.xsd", "DFDL_part2_attributes.xsd", "DFDL_part1_simpletypes.xsd" // IBM supplied. Only minor tweeks by us.
            )),
        new LineCounter("openDFDL", root, List("openDFDL"), List("src/main", "src/test")),
        new LineCounter("daffodil source sbt", root, List("daffodil"), List("project")))),
      new LineCounter("daffodil unit test", root + "daffodil", srcModulesToInclude, List("src/test")),
      new LineCounterCombiner("daffodil tests and examples", List(
        new LineCounter("daffodil tests and examples", root + "daffodil", List("daffodil-test"), List("src/main", "src/test"),
          filesToExclude = List("pcap.tdml")),
        new LineCounter("daffodil 'ibm1' ibm-supplied tests", root + "daffodil", List("daffodil-test-ibm1"),
          List("src/main",
            "src/test/resources/test-suite/ibm-contributed",
            "src/test/scala",
            "src/test/scala-debug"),
          fileSuffixesToInclude = List(".scala", ".java", ".tdml", ".sbt"), // not .xsd as we didn't write the schemas here.
          filesToExclude = List("TresysTests")),
        new LineCounter("daffodil 'ibm1' tresys-supplied tests", root + "daffodil", List("daffodil-test-ibm1"),
          List("src/main",
            "src/test/resources/test-suite/tresys-contributed",
            "src/test/scala",
            "src/test/scala-debug"),
          filesToExclude = List("IBMTests",
            "AT.tdml")) // giant mostly data tdml file.
            )),
      new LineCounterCombiner("Public schemas", List(
        // NACHA is separate due to the 2013 subdir, which we don't have a way to deal with yet
        new LineCounter("DFDLSchemas/NACHA", root + "DFDLSchemas/NACHA", List("2013"), List("src/test")), // only src/test as IBM wrote this schema. We just tweeked it.
        new LineCounter("DFDLSchemas/others", root + "DFDLSchemas", List("mil-std-2045", "png", "bmp", "NITF", "PCAP", "CSV", "jpeg"), List("src/main", "src/test")),
        new LineCounter("jpeg2000", root, List("jpeg2000"), List("src/main", "src/test")),
        new LineCounter("ipfix", root, List("ipfix"), List("")),
        new LineCounter("asterix", root, List("asterix"), List("src/main", "src/test")))),
      new LineCounterCombiner("fouo-schemas", List(
        new LineCounter("fouo-schemas", root + "fouo-schemas", Nil, List("src/main", "src/test"),
          filesToExclude = List(
            "army_drrs_lh.dfdl.tdml", // a giant tdml file that mostly just contains data
            "sets.xsd", "fields.xsd", "messages.xsd", "composites.xsd", // usmtf generated schema
            "link16ed6-daffodil-compliant-original.xsd", "link16ed6-daffodil-compliant-slow.xsd", "link16ed6-daffodil-compliant.xsd", // link16 nato generated schema
            "proposed_ded.dfdl.xsd", "proposed_msg.dfdl.xsd", "dfidui.dfdl.xsd") //
            ),
        new LineCounter("vmf", root + "fouo-schemas/vmf", List("generator", "schema"), List("src/main", "src/test")),
        new LineCounterOneDir("vmf other", root + "fouo-schemas/vmf", List("build.sbt", "README.md") //
        ))), //
      new LineCounterCombiner("Integrations",
        List(
          new LineCounter("calabash proper", root + "daffodil-calabash-extension", List("calabash-server"), List("src/main", "src/test"),
            fileSuffixesToInclude = List(".scala", ".java", ".tdml", ".xsd", ".xpl", ".sbt")),
          new LineCounter("calabash fouo", root + "daffodil-fouo", List("calabash-test"), List("src/main", "src/test"),
            fileSuffixesToInclude = List(".scala", ".java", ".tdml", ".xpl", ".sbt")),
          new LineCounter("NiFi", root + "daffodil-nifi", List("nifi-daffodil-nar", "nifi-daffodil-processors"), List("src/main", "src/test")),
          new LineCounter("Spark", root, List("daffodil-spark"), List("src/main", "src/test")))),
      new LineCounter("all sbt files", root, List("daffodil", "fouo-schemas", "DFDLSchemas", "daffodil-calabash-extension",
        "daffodil-nifi", "jpeg2000", "daffodil-nifi", "daffodil-spark", "ipfix", "asterix"), List(""), fileSuffixesToInclude = List(".sbt")))

    val allpairs = counters.flatMap { _.pairs }
    val nFiles = allpairs.length
    allpairs.foreach { case (fn, cnt) => println(fn.stripPrefix(root), cnt) }
    counters.foreach { cntr => println(cntr.title + " total = " + cntr.total) }
    System.out.println("Grand total = " + counters.map { _.total }.sum + " in " + nFiles + " files.")
  }

  top()
}

trait LineCounterBase {
  def title: String

  def pairs: List[(String, Int)]

  final def total = pairs.map { _._2 }.sum

  protected final def linesWithoutCopyright(f: File): List[String] = {
    val srcContents = Source.fromFile(f).getLines().mkString("\n")
    val srcNoCopyright: String = srcContents.replaceFirst(
      """(?i)(?s)Copyright\s+\(c\)\s+[\w\s-]{0,10}?\s+Tresys\s+Technology""" +
        """.+?\sOTHER.+?DEALINGS.+?WITH.+?THE.+?SOFTWARE\.""",
      "")
    //    if (srcContents == srcNoCopyright)
    //      System.err.println("No banner in " + f.toString)
    //    else
    //      System.err.println("Banner found in " + f.toString)
    val lines: List[String] = srcNoCopyright.split("""\n""").toList
    lines
  }
}

/**
 * Use when you want to combine a bunch of counters that can't be expressed as uniform modules under a root,
 * but you still want them reported as a single count.
 */
class LineCounterCombiner(val title: String, counters: List[LineCounterBase]) extends LineCounterBase {

  lazy val pairs = counters.flatMap { _.pairs }
}

class LineCounterOneDir(val title: String, root: String, files: List[String]) extends LineCounterBase {

  lazy val pairs =
    files.map { fname =>
      {
        val fn = root + "/" + fname
        val f = new File(fn)
        val lines = linesWithoutCopyright(f)
        (fn, lines.length)
      }
    }

}

/**
 * Counts lines of a set of modules under a root.
 */
class LineCounter(
  val title: String,
  root: String,
  modulesToInclude: List[String], // if Nil, uses all directories under the root as modules.
  sourceDirectoriesToInclude: List[String],
  fileSuffixesToInclude: List[String] = LineCounter.fileSuffixesToInclude,
  filesToExclude: List[String] = List())
  extends LineCounterBase {

  /**
   * What to exclude (subversion artifacts, anything generated, version control stuff, caches, settings, etc.)
   */
  private val directoriesToExclude = List(".svn", ".git", ".settings", "lib_managed", "src_managed", "resource_managed", "bin", ".cache", "target", "lib")

  lazy val pairs: List[(String, Int)] = {
    sourceDirectoriesToInclude.flatMap { srcDir => perSourceDir(srcDir) }
  }

  private def countLines(fi: File): Int = {
    val cnt = linesWithoutCopyright(fi).length
    cnt
  }

  private def allSubdirsOfInterest(dir: File): List[File] = {
    //println("allSubdirsOfInterest")
    var res: List[File] = null
    if (!dir.isDirectory()) {
      //println("not a directory")
      res = Nil
    } else {
      //println("Searching for subdirs of interest in: " + dir)
      val toExclude = directoriesToExclude.exists { dirName => dir.getName() == dirName }
      if (toExclude)
        res = Nil
      else {
        val moreDirs = dir.listFiles.toList.filter { _.isDirectory() }.flatMap { allSubdirsOfInterest(_) }
        //println(moreDirs)
        val willKeepDir = filesOfInterest(dir).length > 0
        val result = if (willKeepDir) dir :: moreDirs else moreDirs
        res = result
      }
    }
    res
  }

  private def filesOfInterest(dir: File): List[File] = {
    val files =
      if (!dir.isDirectory()) Nil
      else dir.listFiles.toList.filterNot(_.isDirectory())
    val filesWithoutExcluded = files.filterNot {
      f: File =>
        filesToExclude.exists { ex => f.getName().contains(ex) }
    }
    val filesWithExtensions = filesWithoutExcluded.filter { file =>
      {
        val res = fileSuffixesToInclude.exists { ext => file.getName().endsWith(ext) }
        res
      }
    }
    filesWithExtensions
  }

  private def perSourceDir(srcDir: String): List[(String, Int)] = {
    val files1 = new File(root).listFiles
    if (files1 eq null)
      println(srcDir)
    val topDir = if (files1 eq null) Nil else files1.toList
    val modules =
      if (modulesToInclude != Nil)
        topDir.filter { d: File => modulesToInclude.contains(d.getName()) }
      else {
        // default to all modules
        topDir
      }
    val moduleDirSuffix = if (srcDir != "") "/" + srcDir else ""
    val modulesSrcDirs = modules.map { modDir => new File(modDir + moduleDirSuffix) }
    val modulesDirs = modulesSrcDirs.flatMap { allSubdirsOfInterest(_) }
    val files = modulesDirs.flatMap { filesOfInterest(_) }
    val pairs = files.map { file => (file.toString, countLines(file)) }
    pairs
  }

}
