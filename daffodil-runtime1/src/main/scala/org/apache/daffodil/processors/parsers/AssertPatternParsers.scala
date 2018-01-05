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

package org.apache.daffodil.processors.parsers

import org.apache.daffodil.processors._
import org.apache.daffodil.util.LogLevel
import java.util.regex.Matcher
import org.apache.daffodil.util.OnStack

abstract class AssertPatternParserBase(
  eName: String,
  kindString: String,
  override val context: TermRuntimeData,
  testPattern: String,
  message: String)
  extends PrimParser {
  override lazy val runtimeDependencies: Seq[Evaluatable[AnyRef]] = Nil

  override def toBriefXML(depthLimit: Int = -1) = {
    "<" + kindString + ">" + testPattern + "</" + kindString + ">"
  }

  // private lazy val compiledPattern = ScalaPatternParser.compilePattern(testPattern, context)

  lazy val pattern = ("(?s)" + testPattern).r.pattern // imagine a really big expensive pattern to compile.
  object withMatcher extends OnStack[Matcher](pattern.matcher(""))

  final def parse(start: PState): Unit = {
    val bytePos = (start.bitPos >> 3).toInt
    log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, start.bitPos)
    log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

    log(LogLevel.Debug, "%s - Looking for testPattern = %s", eName, testPattern)

    val dis = start.dataInputStream
    val mark = dis.markPos
    withMatcher { m =>
      val isMatch = dis.lookingAt(m, start)
      afterParse(start, isMatch, m)
    }
    dis.resetPos(mark)
  }

  protected def afterParse(start: PState, isMatch: Boolean, matcher: Matcher): Unit
}

class AssertPatternParser(
  eName: String,
  kindString: String,
  rd: TermRuntimeData,
  testPattern: String,
  message: String)
  extends AssertPatternParserBase(eName, kindString, rd, testPattern, message) {

  def afterParse(start: PState, isMatch: Boolean, matcher: Matcher) {
    if (isMatch) {
      log(LogLevel.Debug, "Assert Pattern success for testPattern %s", testPattern)
    } else {
      log(LogLevel.Debug, "Assert Pattern fail for testPattern %s", testPattern)
      val diag = new AssertionFailed(rd.schemaFileLocation, start, message)
      start.setFailed(diag)
    }
  }
}

class DiscriminatorPatternParser(
  testPattern: String,
  eName: String,
  kindString: String,
  rd: TermRuntimeData,
  message: String)
  extends AssertPatternParserBase(eName, kindString, rd, testPattern, message) {

  def afterParse(start: PState, isMatch: Boolean, matcher: Matcher) {
    if (isMatch) {
      // Only want to set the discriminator if it is true
      // we do not want to modify it unless it's true
      start.setDiscriminator(true)
    } else {
      val diag = new AssertionFailed(rd.schemaFileLocation, start, message)
      start.setFailed(diag)
    }
  }
}
