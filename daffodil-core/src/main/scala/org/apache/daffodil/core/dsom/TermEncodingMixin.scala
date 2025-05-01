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

package org.apache.daffodil.core.dsom

import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.lib.schema.annotation.props.gen.AlignmentKind
import org.apache.daffodil.lib.schema.annotation.props.gen.EncodingErrorPolicy
import org.apache.daffodil.lib.schema.annotation.props.gen.Representation
import org.apache.daffodil.lib.schema.annotation.props.gen.YesNo
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.runtime1.dsom._
import org.apache.daffodil.runtime1.processors.EncodingRuntimeData
import org.apache.daffodil.runtime1.processors.KnownEncodingMixin

/**
 * Captures concepts around dfdl:encoding property and Terms.
 *
 * Just factored out into a trait for isolation of related code.
 */
trait TermEncodingMixin extends KnownEncodingMixin { self: Term =>

  requiredEvaluationsIfActivated(checkTextBidi)

  private lazy val optionTextBidi = findPropertyOption("textBidi")

  private lazy val checkTextBidi =
    (optionTextBidi.isDefined, self.tunable.requireTextBidiProperty) match {
      case (false, false) =>
        SDW(WarnID.TextBidiError, "Property 'dfdl:textBidi' is required but not defined.")
      case (false, true) => textBidi
      case (_, _) =>
        this.subset((textBidi eq YesNo.No), "Property value textBidi='yes' is not supported.")
    }

  protected final lazy val defaultEncodingErrorPolicy = {
    val policy =
      if (self.tunable.requireEncodingErrorPolicyProperty) {
        encodingErrorPolicy
      } else {
        if (!optionEncodingErrorPolicy.isDefined)
          SDW(
            WarnID.EncodingErrorPolicyError,
            "Property 'dfdl:encodingErrorPolicy' is required but not defined, using 'replace' by default."
          )
        optionEncodingErrorPolicy.getOrElse(EncodingErrorPolicy.Replace)
      }

    // DFDL-935 to enable
    schemaDefinitionWarningWhen(
      WarnID.EncodingErrorPolicyError,
      policy == EncodingErrorPolicy.Error,
      "dfdl:encodingErrorPolicy=\"error\" is not yet implemented. The 'replace' value will be used."
    )

    // The encodingErrorPolicy property is essentially ignored. We always use "replace", but
    // warn if it is "error". This allows for compatibility with schemas written for IBM DFDL
    // (which only supports "error") and has the same behavior as IBM DFDL as long as data does
    // not contain encoding errors
    EncodingErrorPolicy.Replace
  }

  /**
   * Character encoding common attributes
   *
   * Note that since encoding can be computed at runtime, we
   * create values to tell us if the encoding is known or not
   * so that we can decide things at compile time when possible.
   */

  final lazy val isKnownEncoding = {
    val isKnown = this.encodingEv.isConstant
    if (isKnown) {
      val encName = encodingEv.optConstant.get.toUpperCase()
      if (encName.startsWith("UTF-16")) {
        utf16Width // demand this so checking is done
      }
    }
    isKnown
  }

  /**
   * When the encoding is known, this tells us the mandatory
   * alignment required. This is always 1 or 8.
   */
  override final lazy val knownEncodingAlignmentInBits = {
    if (alignmentKindDefaulted == AlignmentKind.Manual) 1 // disables any encoding alignment
    else if (isKnownEncoding) {
      schemaDefinitionWarningWhen(
        WarnID.DeprecatedEncodingNameUSASCII7BitPacked,
        knownEncodingName == "US-ASCII-7-BIT-PACKED",
        "Character set encoding name US-ASCII-7-BIT-PACKED is deprecated." +
          "Please update your DFDL schema to use the name X-DFDL-US-ASCII-7-BIT-PACKED."
      )
      val cs = charsetEv.optConstant.get
      cs.mandatoryBitAlignment
    } else 8 // unknown encodings always assumed to be 8-bit aligned.
  }

  lazy val encodingInfo =
    new EncodingRuntimeData(
      charsetEv,
      schemaFileLocation,
      Maybe.toMaybe(optionUTF16Width),
      defaultEncodingErrorPolicy,
      isKnownEncoding,
      isScannable,
      knownEncodingAlignmentInBits,
      hasTextAlignment
    )

  /**
   * True if this element itself consists only of text. No binary stuff like alignment
   * or skips.
   * <p>
   * Not recursive into contained children.
   */
  final lazy val isLocallyTextOnly: Boolean = {
    val res = this match {
      case eb: ElementBase => {
        eb.hasNoSkipRegions &&
        hasTextAlignment &&
        ((eb.isSimpleType && eb.impliedRepresentation == Representation.Text) ||
          eb.isComplexType)
      }
      case mg: ModelGroup => {
        mg.hasNoSkipRegions &&
        hasTextAlignment
      }
    }
    res
  }

  /**
   * True if it is sensible to scan this data e.g., with a regular expression.
   * Requires that all children have same encoding as enclosing groups and
   * elements, requires that there is no leading or trailing alignment regions,
   * skips. We have to be able to determine that we are for sure going to
   * always be properly aligned for text.
   * <p>
   * Caveat: we only care that the encoding is the same if the term
   * actually could have text (couldHaveText is an LV) as part of its
   * representation. For example, a sequence
   * with no initiator, terminator, nor separators can have any encoding at all,
   * without disqualifying an element containing it from being scannable. There
   * has to be text that would be part of the scan.
   * <p>
   * If the root element isScannable, and encodingErrorPolicy is 'replace',
   * then we can use a lower-overhead I/O layer - basically we can use a java.io.InputStreamReader
   * directly.
   * <p>
   * We are going to depend on the fact that if the encoding is going to be this
   * X-DFDL-US-ASCII-7-BIT-PACKED thingy (7-bits wide code units, so aligned at 1 bit) that
   * this encoding must be specified statically in the schema.
   * <p>
   * If an encoding is determined at runtime, then we will
   * insist on it being 8-bit aligned code units.
   */

  final lazy val isScannable: Boolean = {
    if (!isRepresented) true
    else {
      val res = summaryEncoding match {
        case Mixed => false
        case Binary => false
        case NoText => true
        case Runtime => false
        case _ => true
      }
      res
    }
  }

  /**
   * If s1 and s2 are the same encoding name
   * then s1, else "mixed". Also "notext" combines
   * with anything.
   */
  private def combinedEncoding(s1: EncodingLattice, s2: EncodingLattice): EncodingLattice = {
    (s1, s2) match {
      case (x, y) if (x == y) => x
      case (NoText, x) => x
      case (x, NoText) => x
      case _ => Mixed
    }
  }

  /**
   * Roll up from the bottom. This is abstract interpretation.
   * The top (aka conflicting encodings) is "mixed"
   * The bottom is "noText" (combines with anything)
   * The values are encoding names, or "runtime" for expressions.
   * <p>
   * By doing expression analysis we could do a better job
   * here and determine when things that use expressions
   * to get the encoding are all going to get the same
   * expression value. For now, if it is an expression
   * then we lose.
   */
  final lazy val summaryEncoding: EncodingLattice = {
    val myEnc =
      if (!isRepresented) NoText
      else if (!isLocallyTextOnly) Binary
      else if (!couldHaveText) NoText
      else if (!isKnownEncoding) Runtime
      else NamedEncoding(this.knownEncodingName)
    val childEncs: Seq[EncodingLattice] = termChildren.map { x => x.summaryEncoding }
    val res = childEncs.fold(myEnc) { (x, y) => combinedEncoding(x, y) }
    res
  }

  /**
   * True if this term has no alignment properties that would explicitly create
   * a need to align in a way that is not on a suitable boundary
   * for a character.
   *
   * Not the same as AlignedMixin.isKnownToBeTextAligned. That depends on this but
   * goes further to consider whether alignment is achieved even when this is false.
   */
  final lazy val hasTextAlignment = {
    val av = alignmentValueInBits
    val kav = this.knownEncodingAlignmentInBits
    av % kav == 0
  }

}
