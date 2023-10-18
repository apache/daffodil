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

package org.apache.daffodil.core.runtime1

import org.apache.daffodil.core.dsom.ComplexTypeBase
import org.apache.daffodil.core.dsom.ElementBase
import org.apache.daffodil.core.dsom.PrefixLengthQuasiElementDecl
import org.apache.daffodil.core.dsom.PrimitiveType
import org.apache.daffodil.core.dsom.Root
import org.apache.daffodil.core.dsom.SimpleTypeDefBase
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.lib.schema.annotation.props.gen.Representation
import org.apache.daffodil.lib.util.Delay
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.runtime1.dsom.DPathElementCompileInfo
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.RuntimeData
import org.apache.daffodil.runtime1.processors.TermRuntimeData

trait ElementBaseRuntime1Mixin { self: ElementBase =>

  // initialize cyclic structure
  requiredEvaluationsIfActivated(
    dpathElementCompileInfo.initialize,
  )

  // initialize cyclic structure
  requiredEvaluationsIfActivated(
    elementRuntimeData.initialize,
  )

  /**
   * Tells us if, for this element, we need to capture its content length
   *  at unparse runtime, or we can ignore that.
   */
  final lazy val shouldCaptureUnparseContentLength: Boolean = {
    val isReferenced =
      if (this.isInstanceOf[PrefixLengthQuasiElementDecl]) false
      else {
        val setElems = schemaSet.root.contentLengthUnparserReferencedElementInfos
        setElems.contains(this.dpathElementCompileInfo)
      }

    // We need to capture content length when maybeFixedLengthInBits is
    // defined because it allows us to set absolute start bit positions of
    // the DOS, even when there are things like padding and OVC that can
    // cause suspensions that result in relative bit positions. However, we
    // really only need this if there are going to be suspensions, not on all
    // fixed length elements. Otherwise, we're capturing content length for
    // no reason (unless it is referenced in a contentLength expression).
    val mightHaveSuspensions = (maybeFixedLengthInBits.isDefined && couldHaveSuspensions)

    isReferenced || mightHaveSuspensions
  }

  /**
   * Tells us if, for this element, we need to capture its value length
   *  at parse runtime, or we can ignore that.
   */
  final lazy val shouldCaptureParseValueLength: Boolean = {
    val isReferenced =
      if (this.isInstanceOf[PrefixLengthQuasiElementDecl]) false
      else {
        val setElems = schemaSet.root.valueLengthParserReferencedElementInfos
        setElems.contains(this.dpathElementCompileInfo)
      }

    // For simple elements with text representation, value length is captured
    // directly by the value parsers, since those parsers also handle removing
    // delimiters and padding (which must be excluded from value length).
    // Related, simple types that aren't text (e.g. hex binary, packed decimal)
    // but are delimited also use the same simple text value parsers that
    // capture value length.
    //
    // For complex elements with specified length, value length is captured in
    // the specified length parsers, since they handle skipping unused
    // element regions. For complex elements, this means lengthKind is not
    // implicit or delimited.
    //
    // So for these cases we do not want to capture value length with the
    // Capture{Start,End}OfValueLengthParsers, since those lengths are captured
    // by the value parsers
    val capturedByParsers =
      (isSimpleType && (impliedRepresentation == Representation.Text || lengthKind == LengthKind.Delimited)) ||
        (isComplexType && (lengthKind != LengthKind.Implicit && lengthKind != LengthKind.Delimited))

    !capturedByParsers && isReferenced
  }

  /**
   * Tells us if, for this element, we need to capture its value length
   *  at unparse runtime, or we can ignore that.
   */
  final lazy val shouldCaptureUnparseValueLength: Boolean = {
    val isReferenced =
      if (this.isInstanceOf[PrefixLengthQuasiElementDecl]) false
      else {
        val setElems = schemaSet.root.valueLengthUnparserReferencedElementInfos
        setElems.contains(this.dpathElementCompileInfo)
      }

    // Besides being referenced by the dfdl:valueLength function,
    // We need the valueLength to be computed for unparser pad/fill, to check
    // excess length, and for alignmentFills.
    //
    // TBD: why for alignment fills? Don't see using it in the code. Try without this?
    val pad = this.shouldAddPadding
    val fill = this.shouldAddFill
    val len = this.shouldCheckExcessLength
    val alg = !this.isKnownToBeAligned // alignment fill uses the value length.
    val mightHaveSuspensions = pad || fill || len || alg

    isReferenced || mightHaveSuspensions
  }

  final override lazy val dpathCompileInfo = dpathElementCompileInfo

  /**
   * Just an abbrev. analogous to erd, trd, etc.
   */
  final def eci = dpathElementCompileInfo

  /**
   * This is the compile info for this element term.
   */
  lazy val dpathElementCompileInfo: DPathElementCompileInfo = {
    lazy val ee = enclosingElements
    lazy val parents = ee.map {
      _.dpathElementCompileInfo
    }
    val eci = new DPathElementCompileInfo(
      Delay('elementParents, this, parents),
      variableMap,
      Delay('elementChildrenCompileInfo, this, elementChildrenCompileInfo),
      namespaces,
      slashPath,
      name,
      isArray,
      namedQName,
      optPrimType,
      schemaFileLocation,
      tunable.unqualifiedPathStepPolicy,
      shortSchemaComponentDesignator,
      isOutputValueCalc,
      isDistinguishedRoot,
    )
    eci
  }

  private lazy val isDistinguishedRoot = this.isInstanceOf[Root]

  override lazy val runtimeData: RuntimeData = elementRuntimeData
  override lazy val termRuntimeData: TermRuntimeData = elementRuntimeData

  final def erd = elementRuntimeData // just an abbreviation

  private lazy val childrenERDs: Seq[ElementRuntimeData] = LV('childrenERDs) {
    elementChildren.map {
      _.elementRuntimeData
    }
  }.value

  final lazy val elementRuntimeData: ElementRuntimeData = LV('elementRuntimeData) {
    val newERD: ElementRuntimeData = new ElementRuntimeData(
      position,
      childrenERDs,
      schemaSet.variableMap,
      Delay('ElementPartialNextElementResolver, this, partialNextElementResolver),
      encodingInfo,
      dpathElementCompileInfo,
      schemaFileLocation,
      diagnosticDebugName,
      path,
      minimizedScope,
      defaultBitOrder,
      optPrimType,
      targetNamespace,
      optSimpleTypeRuntimeData,
      optComplexTypeModelGroupRuntimeData,
      minOccurs,
      maxOccurs,
      Maybe.toMaybe(optionOccursCountKind),
      name,
      targetNamespacePrefix,
      isNillable,
      isArray, // can have more than 1 occurrence
      isOptional, // can have exactly 0 or 1 occurrence
      isRequiredStreamingUnparserEvent, // must have at least 1 occurrence
      namedQName,
      isRepresented,
      couldHaveText,
      alignmentValueInBits,
      hasNoSkipRegions,
      impliedRepresentation,
      optIgnoreCase,
      defaultValue,
      //
      // unparser specific items
      //
      optTruncateSpecifiedLengthString,
      maybeBinaryFloatRepEv,
      maybeByteOrderEv,
      fillByteEv,
      maybeCheckByteAndBitOrderEv,
      maybeCheckBitOrderAndCharsetEv,
      isQuasiElement,
      runtimeProperties,
    )
    newERD
  }.value

  private lazy val (optSimpleTypeRuntimeData, optComplexTypeModelGroupRuntimeData) =
    typeDef match {
      case _: PrimitiveType => (None, None)
      case ctb: ComplexTypeBase => (None, Some(ctb.modelGroup.modelGroupRuntimeData))
      case s: SimpleTypeDefBase =>
        (Some(s.simpleTypeRuntimeData), None)
    }

}
