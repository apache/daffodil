/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.schema.annotation.props

// Copyright (C) 2012 Michael J. Beckerle. All Rights Reserved.

import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.dsom.EntityReplacer
import edu.illinois.ncsa.daffodil.dsom.ListOfStringValueAsLiteral
import edu.illinois.ncsa.daffodil.dsom.StringValueAsLiteral
import edu.illinois.ncsa.daffodil.dsom.SingleCharacterLiteral
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthUnits
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthUnitsMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.NilKind
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.NilKindMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.NilValueDelimiterPolicyMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.Representation
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.RepresentationMixin
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.OOLAGHost

/**
 * We don't want to make the code generator so sophisticated as to be
 * able to handle these situations.
 *
 * So the code generator has exclusions for these.
 */

sealed trait AlignmentType extends AlignmentType.Value
object AlignmentType extends Enum[AlignmentType] { // Note: Was using AlignmentUnits mixin here!
  case object Implicit extends AlignmentType
  val allowedAlignmentValues = {
    val ints = 0 to 30 // that's every perfect power of 2 that fits in an Int.
    ints.map(1 << _)
  }

  def apply(str: String, self: ThrowsSDE): Any = { // any because it can be an Int, or "implicit"
    if (str == "implicit") return Implicit
    val i =
      try {
        str.toInt
      } catch {
        case e: Throwable => self.schemaDefinitionError("For property 'alignment', value must be 'implicit' or an integer. Found: %s", str)
      }
    if (allowedAlignmentValues.contains(i)) {
      //     val au = alignmentUnits
      //     au match {
      //       case AlignmentUnits.Bits => i // TODO: implement units * Units.bits
      //       case AlignmentUnits.Bytes => i // * Units.bytes
      //     }
      i
    } else self.schemaDefinitionError("For property 'alignment', value must be a power of 2 (and fit in a 32 bit integer). Found: " + str)
  }
}

// Don't need this trait. alignmentType attribute is part of a generated attribute mixin for all the alignment-related attributes
//trait AlignmentTypeMixin extends PropertyMixin {
//  lazy val alignmentType = AlignmentType(getProperty("alignmentType"))
//}

object TextNumberBase {
  def apply(str: String, self: ThrowsSDE): Int = {
    str match {
      case "2" => 2
      case "8" => 8
      case "10" => 10
      case "16" => 16
      case _ => self.schemaDefinitionError("Illegal number base: " + str) // validation will have checked. So this shoudn't happen.
    }
  }

}

sealed trait SeparatorSuppressionPolicy extends SeparatorSuppressionPolicy.Value
object SeparatorSuppressionPolicy extends Enum[SeparatorSuppressionPolicy] {
  case object Never extends SeparatorSuppressionPolicy; forceConstruction(Never)
  case object TrailingEmpty extends SeparatorSuppressionPolicy; forceConstruction(TrailingEmpty)
  case object TrailingEmptyStrict extends SeparatorSuppressionPolicy; forceConstruction(TrailingEmptyStrict)
  case object AnyEmpty extends SeparatorSuppressionPolicy; forceConstruction(AnyEmpty)

  def apply(name: String, self: ThrowsSDE): SeparatorSuppressionPolicy = stringToEnum("separatorSuppressionPolicy", name, self)
}

/**
 * This mixin provides separatorSuppressionPolicy, using that modern
 * property name, but also a legacy property name separatorPolicy that has been superceded now.
 *
 * SeparatorSuppressionPolicyMixin is not auto generated
 * by propgen. Nor is SeparatorPolicyMixin
 */
trait SeparatorPolicyMixin
trait SeparatorSuppressionPolicyMixin
  extends PropertyMixin {

  /**
   * FIXME: really we need to either eliminate separatorPolicy (deprecated name)
   * when loading (by making the SAX event handler isolate and convert/replace it), or
   * we have to make the lookup search for either that name or the new name, which
   * requires that we add a second optional "deprecatedPName" argument to all the lookup primitives
   *
   * This latter capability may be valuable in the future if other new properties are
   * introduced which replace/subsume old properties. I rather expect some evolution of the
   * DFDL standard in this way, so this approach is better than the loader that rewrites. It
   * gives us more flexibility in warnings or error treatment of the old/deprecated names.
   */
  lazy val separatorSuppressionPolicy = {
    val sp = getPropertyOption("separatorPolicy")
    val ssp = getPropertyOption("separatorSuppressionPolicy")
    (sp, ssp) match {
      case (Some(spValue), Some(sspStr)) => {
        SDW("Both separatorPolicy(deprecated) and separatorSuppressionPolicy are defined. The separatorPolicy will be ignored.")
        SeparatorSuppressionPolicy(sspStr, this)
      }
      case (None, Some(sspStr)) => SeparatorSuppressionPolicy(sspStr, this)
      case (None, None) => getProperty("separatorSuppressionPolicy") // which will SDE!
      case (Some(spString), None) => {
        SDW("Property separatorPolicy is deprecated. Use separatorSuppressionPolicy instead.")
        spString match {
          case "required" => SeparatorSuppressionPolicy.Never
          case "suppressed" => SeparatorSuppressionPolicy.AnyEmpty
          case "suppressedAtEndStrict" => SeparatorSuppressionPolicy.TrailingEmpty
          case "suppressedAtEndLax" => SeparatorSuppressionPolicy.TrailingEmptyStrict
          case _ => Assert.invariantFailed("illegal string for separatorPolicy")
        }
      }
      case _ => Assert.invariantFailed("combination of separatorPolicy and separatorSuppressionPolicy not understood")
    }
  }
}

trait TextNumberFormatMixin extends PropertyMixin { self: OOLAGHost =>
  lazy val textStandardInfinityRep = textStandardInfinityRep_.value
  def textStandardInfinityRep_ = LV('textStandardInfinityRep) {
    val raw = getProperty("textStandardInfinityRep")

    this.schemaDefinitionUnless(raw.length() > 0, "textStandardInfinityRep cannot be empty!")
    this.schemaDefinitionUnless(!raw.contains("%NL;"), "textStandardInfinityRep cannot contain NL!")
    this.schemaDefinitionUnless(!raw.contains("%ES;"), "textStandardInfinityRep cannot contain ES!")
    this.schemaDefinitionUnless(!raw.contains("%WSP;"), "textStandardInfinityRep cannot contain WSP!")
    this.schemaDefinitionUnless(!raw.contains("%WSP+;"), "textStandardInfinityRep cannot contain WSP+!")
    this.schemaDefinitionUnless(!raw.contains("%WSP*;"), "textStandardInfinityRep cannot contain WSP*!")
    // TODO: Cannot contain raw bytes

    val l = new StringValueAsLiteral(raw, this)
    l.cooked
  }

  lazy val textStandardNaNRep = {
    val raw = getProperty("textStandardNaNRep")

    this.schemaDefinitionUnless(raw.length() > 0, "textStandardNaNRep cannot be empty!")
    this.schemaDefinitionUnless(!raw.contains("%NL;"), "textStandardNaNRep cannot contain NL!")
    this.schemaDefinitionUnless(!raw.contains("%ES;"), "textStandardNaNRep cannot contain ES!")
    this.schemaDefinitionUnless(!raw.contains("%WSP;"), "textStandardNaNRep cannot contain WSP!")
    this.schemaDefinitionUnless(!raw.contains("%WSP+;"), "textStandardNaNRep cannot contain WSP+!")
    this.schemaDefinitionUnless(!raw.contains("%WSP*;"), "textStandardNaNRep cannot contain WSP*!")
    // TODO: Cannot contain raw bytes

    val l = new StringValueAsLiteral(raw, this)
    l.cooked
  }
  lazy val textStandardZeroRep = {
    val raw = getProperty("textStandardZeroRep")

    // Literal Empty String allowed!
    this.schemaDefinitionUnless(!raw.contains("%NL;"), "textStandardZeroRep cannot contain NL!")
    this.schemaDefinitionUnless(!raw.contains("%ES;"), "textStandardZeroRep cannot contain ES!")
    // TODO: Cannot contain raw bytes

    val l = new ListOfStringValueAsLiteral(raw, this)
    l.cooked
  }
}

trait StringTextMixin extends PropertyMixin
  with LengthUnitsMixin { self: OOLAGHost =>
  lazy val textStringPadCharacterRaw = getProperty("textStringPadCharacter")
  lazy val textStringPadCharacter = textStringPadCharacter_.value
  def textStringPadCharacter_ = LV('textStringPadCharacter) {
    val raw = textStringPadCharacterRaw

    // Can be a literal character or DFDL entity

    // DFDL Character classes are not allowed
    this.schemaDefinitionUnless(!(raw.length() == 0), "textStringPadCharacter cannot be empty!")
    this.schemaDefinitionUnless(!raw.contains("%NL;"), "textStringPadCharacter cannot contain NL!")
    this.schemaDefinitionUnless(!raw.contains("%ES;"), "textStringPadCharacter cannot contain ES!")
    this.schemaDefinitionUnless(!raw.contains("%WSP;"), "textStringPadCharacter cannot contain WSP!")
    this.schemaDefinitionUnless(!raw.contains("%WSP+;"), "textStringPadCharacter cannot contain WSP+!")
    this.schemaDefinitionUnless(!raw.contains("%WSP*;"), "textStringPadCharacter cannot contain WSP*!")

    // TODO: raw byte entity %#r is allowed

    this.lengthUnits match {
      case LengthUnits.Bytes => {
        // TODO: must be single byte value entity
        // TODO: must be single byte character
      }
      case LengthUnits.Characters => {
        // TODO: must be a fixed-width encoding
      }
      case LengthUnits.Bits => this.schemaDefinitionError("textStringPadCharacter lengthUnits cannot be Bits.")
    }

    val l = new SingleCharacterLiteral(raw, this)
    l.cooked
  }
}

trait NumberTextMixin extends PropertyMixin
  with LengthUnitsMixin {
  lazy val textNumberPadCharacterRaw = getProperty("textNumberPadCharacter")
  lazy val textNumberPadCharacter = {
    val raw = textNumberPadCharacterRaw

    // DFDL Character classes are not allowed
    this.schemaDefinitionUnless(!(raw.length() == 0), "textNumberPadCharacter cannot be empty!")
    this.schemaDefinitionUnless(!raw.contains("%NL;"), "textNumberPadCharacter cannot contain NL!")
    this.schemaDefinitionUnless(!raw.contains("%ES;"), "textNumberPadCharacter cannot contain ES!")
    this.schemaDefinitionUnless(!raw.contains("%WSP;"), "textNumberPadCharacter cannot contain WSP!")
    this.schemaDefinitionUnless(!raw.contains("%WSP+;"), "textNumberPadCharacter cannot contain WSP+!")
    this.schemaDefinitionUnless(!raw.contains("%WSP*;"), "textNumberPadCharacter cannot contain WSP*!")

    // TODO: raw byte entity %#r is allowed
    this.lengthUnits match {
      case LengthUnits.Bytes => {
        // TODO: must be single byte value entity
        // TODO: must be single byte character
      }
      case LengthUnits.Characters => {
        // TODO: must be a fixed-width encoding
      }
      case LengthUnits.Bits => this.schemaDefinitionError("textNumberPadCharacter lengthUnits cannot be Bits.")
    }

    val l = new SingleCharacterLiteral(raw, this)
    l.cooked
  }
}

trait BooleanTextMixin extends PropertyMixin
  with LengthUnitsMixin {
  lazy val textBooleanPadCharacterRaw = getProperty("textBooleanPadCharacter")
  lazy val textBooleanPadCharacter = {
    val raw = textBooleanPadCharacterRaw

    // DFDL Character classes are not allowed
    this.schemaDefinitionUnless(!(raw.length() == 0), "textBooleanPadCharacter cannot be empty!")
    this.schemaDefinitionUnless(!raw.contains("%NL;"), "textBooleanPadCharacter cannot contain NL!")
    this.schemaDefinitionUnless(!raw.contains("%ES;"), "textBooleanPadCharacter cannot contain ES!")
    this.schemaDefinitionUnless(!raw.contains("%WSP;"), "textBooleanPadCharacter cannot contain WSP!")
    this.schemaDefinitionUnless(!raw.contains("%WSP+;"), "textBooleanPadCharacter cannot contain WSP+!")
    this.schemaDefinitionUnless(!raw.contains("%WSP*;"), "textBooleanPadCharacter cannot contain WSP*!")

    // TODO: raw byte entity %#r is allowed
    this.lengthUnits match {
      case LengthUnits.Bytes => {
        // TODO: must be single byte value entity
        // TODO: must be single byte character
      }
      case LengthUnits.Characters => {
        // TODO: must be a fixed-width encoding
      }
      case LengthUnits.Bits => this.schemaDefinitionError("textBooleanPadCharacter lengthUnits cannot be Bits.")
    }

    val l = new SingleCharacterLiteral(raw, this)
    l.cooked
  }
}

trait CalendarTextMixin extends PropertyMixin
  with LengthUnitsMixin {
  lazy val textCalendarPadCharacterRaw = getProperty("textCalendarPadCharacter")
  lazy val textCalendarPadCharacter = {
    val raw = textCalendarPadCharacterRaw

    // DFDL Character classes are not allowed
    this.schemaDefinitionUnless(!(raw.length() == 0), "textCalendarPadCharacter cannot be empty!")
    this.schemaDefinitionUnless(!raw.contains("%NL;"), "textCalendarPadCharacter cannot contain NL!")
    this.schemaDefinitionUnless(!raw.contains("%ES;"), "textCalendarPadCharacter cannot contain ES!")
    this.schemaDefinitionUnless(!raw.contains("%WSP;"), "textCalendarPadCharacter cannot contain WSP!")
    this.schemaDefinitionUnless(!raw.contains("%WSP+;"), "textCalendarPadCharacter cannot contain WSP+!")
    this.schemaDefinitionUnless(!raw.contains("%WSP*;"), "textCalendarPadCharacter cannot contain WSP*!")

    // TODO: raw byte entity %#r is allowed
    this.lengthUnits match {
      case LengthUnits.Bytes => {
        // TODO: must be single byte value entity
        // TODO: must be single byte character
      }
      case LengthUnits.Characters => {
        // TODO: must be a fixed-width encoding
      }
      case LengthUnits.Bits => this.schemaDefinitionError("textCalendarPadCharacter lengthUnits cannot be Bits.")
    }

    val l = new SingleCharacterLiteral(raw, this)
    l.cooked
  }
}

trait ImpliedRepresentationMixin extends RepresentationMixin {
  def impliedRepresentation: Representation
}

trait NillableMixin extends PropertyMixin
  with NilKindMixin
  with NilValueDelimiterPolicyMixin
  with ImpliedRepresentationMixin {

  // Information regarding Representation was needed here to characterize
  // appropriate behavior of nilValue
  lazy val nilValue = {
    val raw = getProperty("nilValue")

    this.schemaDefinitionUnless(raw.length() > 0, "nilValue cannot be empty!")

    // Commented out replacement because the replacement takes place within
    // the Parser of StaticText/DynamicText
    this.nilKind match {
      case NilKind.LiteralCharacter => {
        this.schemaDefinitionUnless(!raw.contains("%NL;"), "nilValue cannot contain NL!")
        this.schemaDefinitionUnless(!raw.contains("%ES;"), "nilValue cannot contain ES!")
        this.schemaDefinitionUnless(!raw.contains("%WSP;"), "nilValue cannot contain WSP!")
        this.schemaDefinitionUnless(!raw.contains("%WSP+;"), "nilValue cannot contain WSP+!")
        this.schemaDefinitionUnless(!raw.contains("%WSP*;"), "nilValue cannot contain WSP*!")
        //        val l = new SingleCharacterLiteral(raw, this)
        //        List(l.cooked)
      }
      case NilKind.LogicalValue => {
        this.schemaDefinitionUnless(!raw.contains("%NL;"), "nilValue cannot contain NL!")
        this.schemaDefinitionUnless(!raw.contains("%ES;"), "nilValue cannot contain ES!")
        this.schemaDefinitionUnless(!raw.contains("%WSP;"), "nilValue cannot contain WSP!")
        this.schemaDefinitionUnless(!raw.contains("%WSP+;"), "nilValue cannot contain WSP+!")
        this.schemaDefinitionUnless(!raw.contains("%WSP*;"), "nilValue cannot contain WSP*!")
        //        val l = new ListOfStringValueAsLiteral(raw, this)
        //        l.cooked
      }
      case NilKind.LiteralValue => {
        this.impliedRepresentation match {
          case Representation.Binary => {
            this.schemaDefinitionUnless(!raw.contains("%NL;"), "nilValue can only contain ES!")
            this.schemaDefinitionUnless(!raw.contains("%WSP;"), "nilValue can only contain ES!")
            this.schemaDefinitionUnless(!raw.contains("%WSP+;"), "nilValue can only contain ES!")
            this.schemaDefinitionUnless(!raw.contains("%WSP*;"), "nilValue can only contain ES!")
          }
          case Representation.Text => {}
        }
        //        val l = new ListOfStringValueAsLiteral(raw, this)
        //        l.cooked
      }
    }
    raw
  }
}

