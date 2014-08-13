

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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
 * A very sandbox-structure specific way to count the lines of code in a project.
 */
object LineCounter2 extends App {

  /**
   * exclude generated code from count (we count the generator, not the generated).
   */
  val filesToExclude = List("GeneratedCode.scala")

  /**
   * What directories within modules contain stuff we want to count.
   */
  val sourceDirectoriesToInclude = List("src/main", "src/test")

  /**
   * What to exclude (subversion artifacts, what else)
   */
  val directoriesToExclude = List(".svn", ".git", "lib_managed", "src_managed", "bin", ".cache", "target", "lib")

  /**
   * What file extensions to count.
   */
  val fileSuffixesToInclude = List(".scala") //, ".tdml") // , ".xml") Some test .xml files are Huge. Don't include them.

  /**
   * What modules within the sandbox to count
   */
  val modulesToInclude = List("daffodil-core", "daffodil-lib", "daffodil-io", "daffodil-propgen", "daffodil-cli", "daffodil-tdml", "daffodil-test-ibm1") // , "daffodil-test"

  /**
   * Where is this freakin' sandbox anyway....edit to point to yours.
   */
  val root = "/home/mbeckerle/dataiti/git/daffodil"

  def countLines(fi: File): Int = {
    val src = Source.fromFile(fi)
    val cnt = src.getLines.foldLeft(0) { (i, line) => i + 1 }
    System.out.println(fi.getAbsolutePath() + " line count = " + cnt)
    cnt
  }

  def allSubdirsOfInterest(dir: File): List[File] = {
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

  def filesOfInterest(dir: File): List[File] = {
    val files =
      if (!dir.isDirectory()) Nil
      else dir.listFiles.toList.filterNot(_.isDirectory())
    val filesWithoutExcluded = files.filterNot { f: File => filesToExclude.contains(f.getName()) }
    val filesWithExtensions = filesWithoutExcluded.filter { file =>
      {
        val res = fileSuffixesToInclude.exists { ext => file.getName().endsWith(ext) }
        res
      }
    }
    //println(filesWithExtensions)
    filesWithExtensions
  }

  /**
   * This is the thing that uses all the very sandbox specific paths above.
   */
  def totalPerSourceDir(srcDir: String) = {
    val topDir = new File(root).listFiles.toList
    val modules = topDir.filter { d: File => modulesToInclude.contains(d.getName()) }
    //System.err.println(modules)
    val modulesSrcDirs = modules.map { modDir => new File(modDir + "/" + srcDir) }
    // println(modulesSrcDirs)
    val modulesDirs = modulesSrcDirs.flatMap { allSubdirsOfInterest(_) }
    // println(modulesDirs)
    val files = modulesDirs.flatMap { filesOfInterest(_) }
    val counts = files.map { file => countLines(file) }
    files zip counts foreach println
    val count = counts.sum
    System.out.println("total = " + count)
    count
  }

  def top() {
    val count = sourceDirectoriesToInclude.map { srcDir => totalPerSourceDir(srcDir) }.sum
    System.out.println("grand total = " + count)

    /*
    val counts = sourceDirectoriesToInclude.map { srcDir => filesOfInterest(new File(srcDir)).map { countLines(_) }.sum }
    val res = sourceDirectoriesToInclude zip counts
    res
    */
  }

  top()
}

