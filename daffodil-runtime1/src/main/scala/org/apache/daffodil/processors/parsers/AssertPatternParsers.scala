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

import java.util.regex.Matcher

import org.apache.daffodil.dsom.CompiledExpression
import org.apache.daffodil.dsom.SchemaDefinitionDiagnosticBase
import org.apache.daffodil.processors._
import org.apache.daffodil.util.OnStack
import org.apache.daffodil.lib.schema.annotation.props.gen.FailureType

trait AssertMessageEvaluationMixin {
  def messageExpr: CompiledExpression[AnyRef]
  def discrim: Boolean

  def getAssertFailureMessage(state: PState): String = {
    val message =
      try {
        messageExpr.evaluate(state).asInstanceOf[String]
      } catch {
        // Note that we intentionally catch an SDE here. This effectively
        // converts an SDE (which is usually considered fatal) to a ParseError
        // (which isn't fatal). But section 7.3.1 of the DFDL specification
        // states that both types of errors should be caught and replaced with
        // a recoverable error and an implementation defined replacement
        // message should be provided instead.
        case e @ (_: ParseError | _: SchemaDefinitionDiagnosticBase) => {
          val kindString = if (discrim) "Discriminator" else "Assertion"
          kindString + " message expression evaluation failed: " + e.getMessage
        }
      }
    message
  }
}

class AssertPatternParser(
  override val context: TermRuntimeData,
  override val discrim: Boolean,
  testPattern: String,
  override val messageExpr: CompiledExpression[AnyRef],
  failureType: FailureType)
  extends PrimParser
  with AssertMessageEvaluationMixin {
  override lazy val runtimeDependencies = Vector()

  override def toBriefXML(depthLimit: Int = -1) = {
    val kindString = if (discrim) "Discriminator" else "Assertion"
    "<" + kindString + ">" + testPattern + "</" + kindString + ">"
  }

  lazy val pattern = ("(?s)" + testPattern).r.pattern // imagine a really big expensive pattern to compile.
  object withMatcher extends OnStack[Matcher](pattern.matcher(""))

  final def parse(start: PState): Unit = {
    val bytePos = (start.bitPos >> 3).toInt

    val dis = start.dataInputStream
    val mark = dis.markPos
    val isMatch = withMatcher { m => dis.lookingAt(m, start) }
    dis.resetPos(mark)

    if (!isMatch) {
      val message = getAssertFailureMessage(start)
      if (failureType == FailureType.ProcessingError) {
        val diag = new AssertionFailed(context.schemaFileLocation, start, message)
        start.setFailed(diag)
      } else
        start.SDW(message)
    } else if (discrim) {
      // this is a pattern discriminator. Successful match resolves the in
      // scope point of uncertainty
      start.resolvePointOfUncertainty()
    }
  }
}
