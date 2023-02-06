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
import java.io.File
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.matching.Regex

class Symbols(
  val libraryName: String,
  val fileName: String,
  val packageName: String,
  val exports: List[String]
)

object GenSymbolTable extends App {
  val ignoreFiles = List(
    "daffodil-runtime1/src/main/scala/org/apache/daffodil/reflection/FieldFinder.scala"
  )

  val rePackage: Regex = """package ([\w\.]+)""".r

  def parsePackageName(lines: Iterator[String]): String = {
    for (line <- lines) {
      rePackage.findFirstMatchIn(line) match {
        case Some(name) => return name.group(1)
        case None =>
      }
    }
    throw new Exception("expected package name but not found")
  }

  val reExports = List(
    "^(?:abstract |case |final |sealed |implict |lazy )* ?class ([\\w\\.]+)".r,
    "^(?:abstract |case |final |sealed |implict |lazy )* ?object ([\\w\\.]+)".r,
    "^(?:abstract |case |final |sealed |implict |lazy )* ?trait ([\\w\\.]+)".r
  )

  def parseExports(lines: Iterator[String]): List[String] = {
    val exports = ListBuffer.empty[String]
    for (line <- lines) {
      for (regexp <- reExports) {
        regexp.findFirstMatchIn(line) match {
          case Some(name) => exports += name.group(1)
          case None =>
        }
      }
    }
    exports.toList
  }

  val reLineComment = "//.*".r
  val reBlockStartComment: Regex = """^.*(\/\*).*$""".r
  val reBlockEndComment: Regex = """^.*(\*\/).*$""".r

  def cleanFile(file: File): List[String] = {
    val cleaned = ListBuffer.empty[String]
    val source = Source.fromFile(file)

    var inBlockComment = false
    var exitingBlockComment = false
    for (actual <- source.getLines()) {
      exitingBlockComment = false
      val line = reLineComment.replaceFirstIn(actual, "")
      line match {
        case reBlockStartComment(_) => inBlockComment = true
        case reBlockEndComment(_) => exitingBlockComment = true
        case _ =>
      }
      if (!inBlockComment) cleaned += line
      if (exitingBlockComment) inBlockComment = false
    }
    source.close
    cleaned.toList
  }

  def processFile(libraryName: String, file: File): Option[Symbols] = {
    ignoreFiles.find(file.toString.endsWith(_)) match {
      case Some(_) => return None
      case _ =>
    }
    try {
      val iter = cleanFile(file).iterator
      val packageName = parsePackageName(iter)
      val exports = parseExports(iter)
      Some(new Symbols(libraryName, file.toString, packageName, exports))
    } catch {
      case e: Exception => throw new Exception(s"while processing file: $file", e)
    }
  }

  def processDir(libraryName: String, dir: File): List[Symbols] = {
    val symbols = ListBuffer.empty[Symbols]
    val files = dir.listFiles()
    for (file <- files) {
      val name = file.getName
      if (name.startsWith("Test") || name == "test") {}
      else if (file.isDirectory) symbols ++= processDir(libraryName, file)
      else if (name.endsWith(".scala") || name.endsWith(".java")) {
        processFile(libraryName, file) match {
          case Some(s) => symbols += s
          case _ =>
        }
      }
    }
    symbols.toList
  }

  def processLibrary(dir: File): List[Symbols] = {
    val libraryName = dir.getName
    processDir(libraryName, dir)
  }

  for (arg <- args) {
    val libraryDir = new File(arg)
    val symbols = processLibrary(libraryDir)
    for (symbol <- symbols) {
      for (export <- symbol.exports) println(s"${symbol.libraryName}:${symbol.packageName}.${export}")
    }
  }
}
