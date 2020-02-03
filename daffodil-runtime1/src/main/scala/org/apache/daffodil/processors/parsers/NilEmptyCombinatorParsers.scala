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

import java.io.StringWriter
import java.io.PrintWriter

import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.processors.Success
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.exceptions.UnsuppressableException
import org.apache.daffodil.dsom.SchemaDefinitionDiagnosticBase

abstract class NilOrValueParser(ctxt: TermRuntimeData, nilParser: Parser, valueParser: Parser)
  extends CombinatorParser(ctxt) {

  override lazy val childProcessors = Vector(nilParser, valueParser)
  override lazy val runtimeDependencies = Vector()

  def parse(pstate: PState): Unit = {
    var mark: PState.Mark = pstate.mark("NilOrValueParser")
    var markLeakCausedByException = false

    try {
      nilParser.parse1(pstate)

      if (pstate.processorStatus ne Success) {
        pstate.reset(mark)
        mark = null
        valueParser.parse1(pstate)
      } else {
        pstate.discard(mark)
        mark = null
      }

    } catch {
      // Similar try/catch/finally logic for returning marks is also used in
      // the Sequence parser base. The logic isn't
      // easily factored out so it is duplicated. Changes made here should also
      // be made there. Only these parsers deal with taking marks, so this logic
      // should not be needed elsewhere.
      //
      // TODO: Refactor by hoisting this logic into CombinatorParser using same
      // technique as in SequenceParserBase's tryParseDetectMarkLeaks and parseOne
      // methods. That way this code really can live in exactly one place.
      //
      case t: Throwable => {
        if (mark != null) {
          markLeakCausedByException = true
          if (!t.isInstanceOf[SchemaDefinitionDiagnosticBase] && !t.isInstanceOf[UnsuppressableException] && !t.isInstanceOf[java.lang.Error]) {
            val stackTrace = new StringWriter()
            t.printStackTrace(new PrintWriter(stackTrace))
            Assert.invariantFailed("Exception thrown with mark not returned: " + t + "\nStackTrace:\n" + stackTrace)
          }
        }
        throw t
      }
    } finally {
      var markLeak = false;
      if (mark != null) {
        pstate.discard(mark)
        markLeak = true;
      }

      if (markLeak && !markLeakCausedByException) {
        // likely a logic bug, throw assertion
        Assert.invariantFailed("mark not returned, likely a logic bug")
      }
    }
  }
}


case class SimpleNilOrValueParser(ctxt: TermRuntimeData, nilParser: Parser, valueParser: Parser)
  extends NilOrValueParser(ctxt, nilParser, valueParser)

case class ComplexNilOrContentParser(ctxt: TermRuntimeData, emptyParser: Parser, contentParser: Parser)
  extends NilOrValueParser(ctxt, emptyParser, contentParser)
