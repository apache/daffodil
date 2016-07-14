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
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.dsom
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.OOLAGHost
import java.lang.{ Integer => JInt }
import edu.illinois.ncsa.daffodil.util.MaybeULong
import passera.unsigned.ULong


/**
 * We don't want to make the code generator so sophisticated as to be
 * able to handle these situations.
 *
 * So the code generator has exclusions for these.
 */

sealed trait AlignmentType extends AlignmentType.Value
object AlignmentType extends Enum[AnyRef] { // Note: Was using AlignmentUnits mixin here!
  case object Implicit extends AlignmentType
  val allowedAlignmentValues = {
    val ints = 0 to 30 // that's every perfect power of 2 that fits in an Int.
    ints.map(1 << _)
  }

  def apply(str: String, self: ThrowsSDE): AnyRef = { // any because it can be an Int, or "implicit"
    if (str == "implicit") return Implicit
    val i: Int =
      try {
        str.toInt
      } catch {
        case e: NumberFormatException => self.schemaDefinitionError("For property 'alignment', value must be 'implicit' or an integer. Found: %s", str)
      }
    if (allowedAlignmentValues.contains(i)) {
      //     val au = alignmentUnits
      //     au match {
      //       case AlignmentUnits.Bits => i // TODO: implement units * Units.bits
      //       case AlignmentUnits.Bytes => i // * Units.bytes
      //     }
      new JInt(i)
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

  lazy val textStandardInfinityRep = {
    val raw = getProperty("textStandardInfinityRep")
    val cooked = TextStandardInfinityRepCooker.convertConstant(raw, this, false)
    cooked
  }

  lazy val textStandardNaNRep = {
    val raw = getProperty("textStandardNaNRep")
    val cooked = TextStandardNaNRepCooker.convertConstant(raw, this, false)
    cooked
  }

  lazy val textStandardZeroRep = {
    val raw = getProperty("textStandardZeroRep")
    val cooked = TextStandardZeroRepCooker.convertConstant(raw, this, false)
    cooked
  }
}

trait StringTextMixin extends PropertyMixin
  with LengthUnitsMixin { self: OOLAGHost =>

  lazy val textStringPadCharacterRaw = getProperty("textStringPadCharacter")

  lazy val textStringPadCharacter = {
    val raw = textStringPadCharacterRaw
    val cooked = TextStringPadCharacterCooker.convertConstant(raw, this, false)

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
    cooked
  }
}

trait NumberTextMixin extends PropertyMixin
  with LengthUnitsMixin {

  lazy val textNumberPadCharacterRaw = getProperty("textNumberPadCharacter")

  lazy val textNumberPadCharacter = {
    val raw = textNumberPadCharacterRaw
    val cooked = dsom.TextNumberPadCharacterCooker.convertConstant(raw, this, forUnparse = false)

    this.lengthUnits match {
      case LengthUnits.Bytes => {
        // TODO: must be single byte value entity
        // TODO: must be single byte character - means this can't be done statically like this.
        // because it depends (or could) on encoding, just for the checking.
      }
      case LengthUnits.Characters => {
        // TODO: must be a fixed-width encoding
      }
      case LengthUnits.Bits => this.schemaDefinitionError("textNumberPadCharacter lengthUnits cannot be Bits.")
    }

    cooked
  }
}

trait BooleanTextMixin extends PropertyMixin
  with LengthUnitsMixin {

  lazy val textBooleanPadCharacterRaw = getProperty("textBooleanPadCharacter")

  lazy val textBooleanPadCharacter = {
    val raw = textBooleanPadCharacterRaw
    val cooked = TextBooleanPadCharacterCooker.convertConstant(raw, this, false)

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

    cooked
  }
}

trait CalendarTextMixin extends PropertyMixin
  with LengthUnitsMixin {

  lazy val textCalendarPadCharacterRaw = getProperty("textCalendarPadCharacter")

  lazy val textCalendarPadCharacter = {
    val raw = textCalendarPadCharacterRaw
    val cooked = TextCalendarPadCharacterCooker.convertConstant(raw, this, false)
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
    cooked
  }
}

trait ImpliedRepresentationMixin extends RepresentationMixin {
  def impliedRepresentation: Representation
}

trait NillableMixin extends PropertyMixin
  with NilKindMixin
  with NilValueDelimiterPolicyMixin
  with ImpliedRepresentationMixin {

  private lazy val rawNilValue = getProperty("nilValue")

  // Information regarding Representation was needed here to characterize
  // appropriate behavior of nilValue
  def cookedNilValue(forUnparse: Boolean): List[String] = {
    val raw = rawNilValue
    val res =
      this.nilKind match {
        case NilKind.LiteralCharacter => {
          val cooker = NilValueLiteralCharacterCooker
          val cooked = cooker.convertConstant(raw, this, forUnparse)
          List(cooked)
        }
        case _ => {
          val cooker = nilKind match {
            case NilKind.LogicalValue => NilValueLogicalValueCooker
            case NilKind.LiteralValue => {
              this.impliedRepresentation match {
                case Representation.Binary => NilValueLiteralValueBinaryCooker
                case Representation.Text => NilValueLiteralValueTextCooker
              }
            }
            case NilKind.LiteralCharacter => Assert.impossibleCase
          }
          val cooked = cooker.convertConstant(raw, this, forUnparse)
          cooked
        }
      }
    Assert.invariant(res.length > 0)
    if (res.length =#= 1) res
    else if (forUnparse) res.take(1)
    else res
  }

  def rawNilValueList(forUnparse: Boolean) = {
    NilValueRawListCooker.convertConstant(rawNilValue, this, forUnparse)
  }

}

object BinaryBooleanTrueRepType {

  def apply(str: String, element: ThrowsSDE): MaybeULong = {
    if (str == "") {
      MaybeULong.Nope
    } else {
      val i: Int =
        try {
          str.toInt
        } catch {
          case e: NumberFormatException => element.schemaDefinitionError("For property 'binaryBooleanTrueRep', value must be an empty string or a non-negative integer. Found: %s", str)
        }
  
      if(i < 0) element.schemaDefinitionError("For property 'binaryBooleanFalseRep', value must be an empty string or a non-negative integer. Found: %d", i)
      MaybeULong(i)
    }
  }
}

object BinaryBooleanFalseRepType {

  def apply(str: String, element: ThrowsSDE): ULong = {
    val i: Int =
      try {
        str.toInt
      } catch {
        case e: NumberFormatException => element.schemaDefinitionError("For property 'binaryBooleanFalseRep', value must be an integer. Found: %s", str)
      }

    if(i < 0) element.schemaDefinitionError("For property 'binaryBooleanFalseRep', value must be a non-negative integer. Found: %d", i)
    ULong(i)
  }
}
