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

import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.runtime1.dpath.NumberCompareOp
import org.apache.daffodil.runtime1.infoset.DataValue
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueNumber
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitiveNullable
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueString
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Success

/*
 * Run a parser for an element that does not occur in the infoset
 * Prior to running, a temporary element (of the type expected by the parser) will be created in the infoset,
 * After running, the infoset will be reverted to its original state, but any other side effect of parsing will remain
 *
 * Additionally, the dataValue of the element the parser parsed will be returned
 */
trait WithDetachedParser {
  def runDetachedParser(
    pstate: PState,
    detachedParser: Parser,
    erd: ElementRuntimeData
  ): DataValuePrimitiveNullable = {
    /*
     * The parse1 being called here is that of ElementCombinator1, which expects to begin and end in the parent
     * of whatever element it is parsing. parse1 will create the new element and append it to the end of the
     * children list of the parent.
     *
     * The parse() call we are in currently is in the middle of the above process already.
     * To use the detachedParser, we need to unwind then rewind the work that ElementCombinator1 has already done
     *  (in addition to reverting the infoset changes that repTypeParser made). the general flow is:
     *
     *  1) priorElement = pstate.infoset
     *  2) pstate.infoset = pstate.infoset.parent
     *  3) pstate.infoset.mark
     *  4) distachedParser.parse1
     *  5) pstate.infoset.restore
     *  6) pstate.infoset = priorElement
     *
     *  Note that we are only restoring the infoset. Any other side effects the repTypeParser has on pstate
     *  (such as advancing the bit posistion) will remain.
     *
     *  If repTypeParser has an error (either thrown or status), we percolate it up to our caller.
     */

    val priorElement = pstate.infoset
    val priorElementLastChild = pstate.infosetLastChild
    pstate.setInfoset(pstate.infoset.diParent, Nope)

    // This isn't actually a point of uncertainty, we just use the logic to
    // allow resetting the infoset after we create the detached parser
    val ans = pstate.withPointOfUncertainty("WithDetachedParser", erd) { pou =>
      detachedParser.parse1(pstate)

      val res: DataValuePrimitiveNullable = pstate.processorStatus match {
        case Success => pstate.infoset.child(pstate.infoset.numChildren - 1).asSimple.dataValue
        case _ => DataValue.NoValue
      }

      // Restore the infoset. withPointOfUncertainty will discard the pou when
      // this block ends, thus keeping the rest of the modified state
      pou.restoreInfoset(pstate)

      res
    }

    pstate.setInfoset(priorElement, priorElementLastChild)

    ans
  }
}

class RepTypeParser(
  repTypeParser: Parser,
  e: ElementRuntimeData,
  repTypeRuntimeData: ElementRuntimeData,
  repValuesMap: Map[DataValueNumber, DataValueString],
  repValueRangesMap: Seq[(DataValueNumber, DataValueNumber, DataValueString)],
  lt: NumberCompareOp,
  le: NumberCompareOp
) extends CombinatorParser(e)
  with WithDetachedParser {

  override def childProcessors = Vector(repTypeParser)
  override def runtimeDependencies = Vector()

  override def parse(pstate: PState): Unit = {
    val repValue =
      runDetachedParser(pstate, repTypeParser, repTypeRuntimeData).asInstanceOf[DataValueNumber]
    if (pstate.processorStatus == Success) {
      val optLogical: Option[DataValueString] = None
        .orElse(repValuesMap.get(repValue))
        .orElse(
          repValueRangesMap
            .find { case (low, high, _) =>
              !lt.operate(repValue, low).getBoolean && le.operate(repValue, high).getBoolean
            }
            .map(_._3)
        )
      optLogical match {
        case Some(logical) => pstate.simpleElement.setDataValue(logical)
        case None =>
          PE(
            pstate,
            "Value %s not found in enumeration dfdlx:repValues or dfdlx:repValueRanges",
            repValue.value
          )
      }
    }
  }

}
