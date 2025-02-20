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

package org.apache.daffodil.unparsers.runtime1

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.lib.schema.annotation.props.gen.Representation
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.runtime1.infoset.DIElement
import org.apache.daffodil.runtime1.infoset.DISimple
import org.apache.daffodil.runtime1.infoset.Infoset
import org.apache.daffodil.runtime1.processors.CharsetEv
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.UnparseTargetLengthInBitsEv
import org.apache.daffodil.runtime1.processors.unparsers._

import passera.unsigned.ULong

final class SpecifiedLengthExplicitImplicitUnparser(
  eUnparser: Unparser,
  erd: ElementRuntimeData,
  targetLengthInBitsEv: UnparseTargetLengthInBitsEv
) extends CombinatorUnparser(erd) {

  override def runtimeDependencies = Vector()

  override def childProcessors = Vector(eUnparser)

  private val libEv = targetLengthInBitsEv.lengthInBitsEv
  private val mcsEv = libEv.maybeCharsetEv
  private val lengthUnits = libEv.lengthUnits
  private val lengthKind = libEv.lengthKind

  private def getCharset(state: UState) = {
    val csEv: CharsetEv = mcsEv.get
    val dcs = csEv.evaluate(state)
    dcs
  }

  override final def unparse(state: UState): Unit = {
    lazy val dcs = getCharset(state)
    if (
      erd.impliedRepresentation == Representation.Text &&
      lengthUnits == LengthUnits.Characters &&
      dcs.maybeFixedWidth.isEmpty &&
      erd.isComplexType
    ) {
      state.subsetError(
        "Variable width character encoding '%s', dfdl:lengthKind '%s' and dfdl:lengthUnits '%s' are not supported for complex types.",
        getCharset(state).name,
        lengthKind.toString,
        lengthUnits.toString
      )
    } else {
      eUnparser.unparse1(state)
    }
  }
}

/**
 * This trait is to be used with prefixed length unparsers where the length
 * must be calculated based on the content length of the data. This means the
 * data must be unparsed, the content length calculated, and that value will be
 * assigned to the prefix length element.
 */
trait CalculatedPrefixedLengthUnparserMixin {
  def lengthUnits: LengthUnits
  def prefixedLengthAdjustmentInUnits: Long

  /**
   * Gets the length of a provided element and stores the unit adjusted length
   * as the value in the element that represent its prefix length
   *
   * @param elem The element who's length to get
   * @param plElem The element to store the length
   * @param lengthUnits The length units (bytes or bits)
   */
  def assignPrefixLength(state: UState, elem: DIElement, plElem: DISimple): Unit = {
    val lenInUnits = lengthUnits match {
      case LengthUnits.Bits => elem.contentLength.lengthInBits
      case LengthUnits.Bytes => elem.contentLength.lengthInBytes
      case LengthUnits.Characters => {
        val maybeFixedWidth =
          elem.erd.encInfo.getEncoderInfo(state).coder.bitsCharset.maybeFixedWidth
        val lengthInChars =
          if (maybeFixedWidth.isDefined) {
            val fixedWidth = maybeFixedWidth.get
            Assert.invariant((elem.contentLength.lengthInBits % fixedWidth) == 0) // divisible
            elem.contentLength.lengthInBits / fixedWidth
          } else {
            // This is checked for statically, so should not get here.
            // $COVERAGE-OFF$
            Assert.invariantFailed(
              "Not supported: prefixed length with variable-width or non-constant encoding."
            )
            // $COVERAGE-ON$
          }
        ULong(lengthInChars)
      }
    }
    val adjustedLenInUnits = lenInUnits + prefixedLengthAdjustmentInUnits
    plElem.setDataValue(java.lang.Integer.valueOf(adjustedLenInUnits.toInt))
    // do checks on facets expressed on prefixLengthType
    val optSTRD = plElem.erd.optSimpleTypeRuntimeData
    if (optSTRD.isDefined) {
      val strd = optSTRD.get
      val check = strd.executeCheck(plElem)
      if (check.isError) {
        UnparseError(
          One(state.schemaFileLocation),
          One(state.currentLocation),
          s"The prefix length value of ${elem.namedQName} ($adjustedLenInUnits) failed check due to ${check.errMsg}"
        )
      }
    }
  }
}

class SpecifiedLengthPrefixedUnparser(
  eUnparser: Unparser,
  erd: ElementRuntimeData,
  prefixedLengthUnparser: Unparser,
  prefixedLengthERD: ElementRuntimeData,
  override val lengthUnits: LengthUnits,
  override val prefixedLengthAdjustmentInUnits: Long
) extends CombinatorUnparser(erd)
  with CalculatedPrefixedLengthUnparserMixin {

  override def runtimeDependencies = Vector()

  override def childProcessors = Vector(prefixedLengthUnparser, eUnparser)

  override def unparse(state: UState): Unit = {
    // Create a "detached" DIDocument with a single child element that the
    // prefix length will be parsed to. This creates a completely new
    // infoset and parses to that, so care is taken to ensure this infoset
    // is only used for the prefix length parsing and is removed afterwards
    val plElem = Infoset.newDetachedElement(state, prefixedLengthERD).asInstanceOf[DISimple]

    // The prefixedLengthUnparser is going to end up creating a suspension
    // because plElem does not have a value yet. We will temporary push the
    // detached element to the stack, the suspension will clone the ustate and
    // keep track of that detached element, then we can pop it off the stack as
    // it's no longer needed.
    state.currentInfosetNodeStack.push(One(plElem))
    prefixedLengthUnparser.unparse1(state)
    state.currentInfosetNodeStack.pop

    val elem = state.currentInfosetNode.asInstanceOf[DIElement]
    eUnparser.unparse1(state)

    if (elem.contentLength.maybeLengthInBits().isDefined) {
      // If we were able to immediately calculate the length of the element,
      // then just set it as the value of the detached element created above so
      // that when the prefixedLengthUnparser suspension resumes it can unparse
      // the value
      assignPrefixLength(state, elem, plElem)
    } else {
      // The length was not able to be calculated, likely because there was a
      // suspension when unparsing the eUnparser. So let's create a new
      // suspension with the only goal to retry until the contentLength of this
      // element is determined. Once determined, it will set the value of the
      // prefix length element, ultimately allowing the prefix length element
      // suspension to resume and unparse the value
      val suspension = new PrefixLengthSuspendableOperation(
        erd,
        elem,
        plElem,
        lengthUnits,
        prefixedLengthAdjustmentInUnits
      )

      // Run the suspension--we know the suspension will not be able to succeed
      // since maybeLengthInBits is not defined, but this performs various
      // actions to suspend the operation and allow it to be run later
      suspension.run(state)
    }
  }

}
