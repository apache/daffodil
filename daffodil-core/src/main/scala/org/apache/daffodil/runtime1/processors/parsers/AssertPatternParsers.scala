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

package org.apache.daffodil.runtime1.processors.parsers

import java.util.regex.Matcher

import org.apache.daffodil.lib.schema.annotation.props.gen.FailureType
import org.apache.daffodil.lib.util.OnStack
import org.apache.daffodil.runtime1.dsom.CompiledExpression
import org.apache.daffodil.runtime1.dsom.SchemaDefinitionDiagnosticBase
import org.apache.daffodil.runtime1.processors._

trait AssertParserMixin {
  def messageExpr: CompiledExpression[AnyRef]
  def discrim: Boolean
  def failureType: FailureType

  def handleAssertionResult(res: Boolean, state: PState, context: RuntimeData): Unit = {
    if (!res) {
      val message = getAssertFailureMessage(state)
      if (failureType == FailureType.ProcessingError) {
        val diag = new AssertionFailed(context.schemaFileLocation, state, message)
        state.setFailed(diag)
      } else
        state.validationError("%s", message)
    } else if (discrim) {
      // this is a discriminator. Successful assertion resolves the in scope
      // point of uncertainty
      state.resolvePointOfUncertainty()
    }
  }

  private def getAssertFailureMessage(state: PState): String = {
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
  override val failureType: FailureType
) extends PrimParser
  with AssertParserMixin {
  override def runtimeDependencies = Vector()

  override def toBriefXML(depthLimit: Int = -1) = {
    val kindString = if (discrim) "Discriminator" else "Assertion"
    "<" + kindString + ">" + testPattern + "</" + kindString + ">"
  }

  lazy val pattern =
    ("(?s)" + testPattern).r.pattern // imagine a really big expensive pattern to compile.
  object withMatcher extends OnStack[Matcher](pattern.matcher(""))

  final def parse(start: PState): Unit = {
    val bytePos = (start.bitPos >> 3).toInt

    val dis = start.dataInputStream
    val mark = dis.markPos
    val isMatch = withMatcher { m => dis.lookingAt(m, start) }
    dis.resetPos(mark)

    handleAssertionResult(isMatch, start, context)
  }
}
