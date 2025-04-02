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

package org.apache.daffodil.lib.schema.annotation.props

import java.lang.{ Integer => JInt }

import org.apache.daffodil.lib.cookers.NilValueLiteralCharacterCooker
import org.apache.daffodil.lib.cookers.NilValueLiteralValueBinaryCooker
import org.apache.daffodil.lib.cookers.NilValueLiteralValueTextCooker
import org.apache.daffodil.lib.cookers.NilValueLogicalValueCooker
import org.apache.daffodil.lib.cookers.NilValueRawListCooker
import org.apache.daffodil.lib.cookers.TextBooleanPadCharacterCooker
import org.apache.daffodil.lib.cookers.TextCalendarPadCharacterCooker
import org.apache.daffodil.lib.cookers.TextNumberPadCharacterCooker
import org.apache.daffodil.lib.cookers.TextStandardInfinityRepCooker
import org.apache.daffodil.lib.cookers.TextStandardNaNRepCooker
import org.apache.daffodil.lib.cookers.TextStandardZeroRepCooker
import org.apache.daffodil.lib.cookers.TextStringPadCharacterCooker
import org.apache.daffodil.lib.equality.ViewEqual
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.lib.iapi.DaffodilTunables
import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.lib.oolag.OOLAG.OOLAGHost
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthUnitsMixin
import org.apache.daffodil.lib.schema.annotation.props.gen.NilKind
import org.apache.daffodil.lib.schema.annotation.props.gen.NilKindMixin
import org.apache.daffodil.lib.schema.annotation.props.gen.NilValueDelimiterPolicyMixin
import org.apache.daffodil.lib.schema.annotation.props.gen.Representation
import org.apache.daffodil.lib.schema.annotation.props.gen.RepresentationMixin
import org.apache.daffodil.lib.util.MaybeULong

import passera.unsigned.ULong

/**
 * We don't want to make the code generator so sophisticated as to be
 * able to handle these situations.
 *
 * So the code generator has exclusions for these.
 */

sealed trait AlignmentType extends EnumValue
object AlignmentType extends Enum[AnyRef] { // Note: Was using AlignmentUnits mixin here!
  case object Implicit extends AlignmentType
  override lazy val values = Array(Implicit)

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
        case e: NumberFormatException =>
          self.schemaDefinitionError(
            "For property 'alignment', value must be 'implicit' or an integer. Found: %s",
            str
          )
      }
    if (allowedAlignmentValues.contains(i)) {
      //     val au = alignmentUnits
      //     au match {
      //       case AlignmentUnits.Bits => i // TODO: implement units * Units.bits
      //       case AlignmentUnits.Bytes => i // * Units.bytes
      //     }
      JInt.valueOf(i)
    } else
      self.schemaDefinitionError(
        "For property 'alignment', value must be a power of 2 (and fit in a 32 bit integer). Found: " + str
      )
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
      case _ =>
        self.schemaDefinitionError(
          "For property textStandardBase, value must be 2, 8, 10, or 16. Found: %s",
          str
        )
    }
  }
}
trait TextStandardBaseMixin extends PropertyMixin {

  def tunable: DaffodilTunables

  private def optionTextStandardBase = findPropertyOption("textStandardBase")

  /**
   * Daffodil 2.5.0 and older ignored the textStandardBase property, behaving
   * as if the value was always 10. Newer versions of Daffodil support this
   * property, but we don't want to require it and potentially break old
   * schemas that do not define it. So we check if we should require the
   * property based on a tunable, and if we shouldn't require it and it's not
   * defined, then we warn and default to 10.
   */
  final lazy val textStandardBaseDefaulted = {
    val numStr =
      if (tunable.requireTextStandardBaseProperty || optionTextStandardBase.isDefined) {
        getProperty("textStandardBase")
      } else {
        SDW(
          WarnID.TextStandardBaseUndefined,
          "dfdl:textStandardBase property is undefined. Defaulting to 10."
        )
        "10"
      }
    TextNumberBase(numStr, this)
  }
}

sealed trait SeparatorSuppressionPolicy extends EnumValue
object SeparatorSuppressionPolicy extends Enum[SeparatorSuppressionPolicy] {
  case object Never extends SeparatorSuppressionPolicy
  case object TrailingEmpty extends SeparatorSuppressionPolicy
  case object TrailingEmptyStrict extends SeparatorSuppressionPolicy
  case object AnyEmpty extends SeparatorSuppressionPolicy
  override lazy val values = Array(Never, TrailingEmpty, TrailingEmptyStrict, AnyEmpty)

  def apply(name: String, self: ThrowsSDE): SeparatorSuppressionPolicy =
    stringToEnum("separatorSuppressionPolicy", name, self)
}

/**
 * This mixin provides separatorSuppressionPolicy, using that modern
 * property name, but also a legacy property name separatorPolicy that has been superceded now.
 *
 * SeparatorSuppressionPolicyMixin is not auto generated
 * by propgen. Nor is SeparatorPolicyMixin
 */
trait SeparatorPolicyMixin
trait SeparatorSuppressionPolicyMixin extends PropertyMixin {

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
  lazy val separatorSuppressionPolicy: SeparatorSuppressionPolicy = {
    val sp = getPropertyOption("separatorPolicy")
    val ssp = getPropertyOption("separatorSuppressionPolicy")
    (sp, ssp) match {
      case (Some(spValue), Some(sspStr)) => {
        SDW(
          WarnID.DeprecatedPropertySeparatorPolicy,
          "Both separatorPolicy(deprecated) and separatorSuppressionPolicy are defined. The separatorPolicy will be ignored."
        )
        SeparatorSuppressionPolicy(sspStr, this)
      }
      case (None, Some(sspStr)) => SeparatorSuppressionPolicy(sspStr, this)
      case (None, None) => {
        getProperty("separatorSuppressionPolicy") // which will SDE!
        Assert.impossible("Above is going to SDE")
      }
      case (Some(spString), None) => {
        SDW(
          WarnID.DeprecatedPropertySeparatorPolicy,
          "Property separatorPolicy is deprecated. Use separatorSuppressionPolicy instead."
        )
        spString match {
          case "required" => SeparatorSuppressionPolicy.Never
          case "suppressed" => SeparatorSuppressionPolicy.AnyEmpty
          case "suppressedAtEndLax" => SeparatorSuppressionPolicy.TrailingEmpty
          case "suppressedAtEndStrict" => SeparatorSuppressionPolicy.TrailingEmptyStrict
          case _ => Assert.invariantFailed("illegal string for separatorPolicy")
        }
      }
      case _ =>
        Assert.invariantFailed(
          "combination of separatorPolicy and separatorSuppressionPolicy not understood"
        )
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

trait StringTextMixin extends PropertyMixin with LengthUnitsMixin { self: OOLAGHost =>

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
      case LengthUnits.Bits =>
        this.schemaDefinitionError("textStringPadCharacter lengthUnits cannot be Bits.")
    }
    cooked
  }
}

trait NumberTextMixin extends PropertyMixin with LengthUnitsMixin {

  lazy val textNumberPadCharacterRaw = getProperty("textNumberPadCharacter")

  lazy val textNumberPadCharacter = {
    val raw = textNumberPadCharacterRaw
    val cooked = TextNumberPadCharacterCooker.convertConstant(raw, this, forUnparse = false)

    this.lengthUnits match {
      case LengthUnits.Bytes => {
        // TODO: must be single byte value entity
        // TODO: must be single byte character - means this can't be done statically like this.
        // because it depends (or could) on encoding, just for the checking.
      }
      case LengthUnits.Characters => {
        // TODO: must be a fixed-width encoding
      }
      case LengthUnits.Bits =>
        this.schemaDefinitionError("textNumberPadCharacter lengthUnits cannot be Bits.")
    }

    cooked
  }
}

trait BooleanTextMixin extends PropertyMixin with LengthUnitsMixin {

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
      case LengthUnits.Bits =>
        this.schemaDefinitionError("textBooleanPadCharacter lengthUnits cannot be Bits.")
    }

    cooked
  }
}

trait CalendarTextMixin extends PropertyMixin with LengthUnitsMixin {

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
      case LengthUnits.Bits =>
        this.schemaDefinitionError("textCalendarPadCharacter lengthUnits cannot be Bits.")
    }
    cooked
  }
}

trait ImpliedRepresentationMixin extends RepresentationMixin {
  def impliedRepresentation: Representation
}

trait NillableMixin
  extends PropertyMixin
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
          case e: NumberFormatException =>
            element.schemaDefinitionError(
              "For property 'binaryBooleanTrueRep', value must be an empty string or a non-negative integer. Found: %s",
              str
            )
        }

      if (i < 0)
        element.schemaDefinitionError(
          "For property 'binaryBooleanFalseRep', value must be an empty string or a non-negative integer. Found: %d",
          i
        )
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
        case e: NumberFormatException =>
          element.schemaDefinitionError(
            "For property 'binaryBooleanFalseRep', value must be an integer. Found: %s",
            str
          )
      }

    if (i < 0)
      element.schemaDefinitionError(
        "For property 'binaryBooleanFalseRep', value must be a non-negative integer. Found: %s",
        i
      )
    ULong(i)
  }
}

/**
 * This mixin provides textStandardExponentRep, using that modern
 * property name, but also a legacy property name textStandardExponentCharacter that has been superceded now.
 *
 * TextStandardExponentRepMixin is not auto generated
 * by propgen. Nor is TextStandardExponentCharacterMixin
 */
trait TextStandardExponentCharacterMixin

trait TextStandardExponentRepMixin extends PropertyMixin {
  protected final lazy val optionTextStandardExponentRepRaw =
    findPropertyOption("textStandardExponentRep", expressionAllowed = true)
  protected final lazy val textStandardExponentRepRaw = requireProperty(
    optionTextStandardExponentRepRaw
  )

  // Deprecated textStandardExponentCharacter
  protected final lazy val optionTextStandardExponentCharacterRaw = findPropertyOption(
    "textStandardExponentCharacter"
  )
  protected final lazy val textStandardExponentCharacterRaw = requireProperty(
    optionTextStandardExponentCharacterRaw
  )

  lazy val textStandardExponentRep: Found = {
    val tsec = getPropertyOption("textStandardExponentCharacter")
    val tser = getPropertyOption("textStandardExponentRep", expressionAllowed = true)
    (tsec, tser) match {
      case (Some(tsecStr), Some(tserStr)) => {
        SDW(
          WarnID.DeprecatedPropertySeparatorPolicy,
          "Both textStandardExponentCharacter(deprecated) and textStandardExponentRep are defined. The textStandardExponentCharacter will be ignored."
        )
        textStandardExponentRepRaw
      }
      case (None, Some(tserStr)) => textStandardExponentRepRaw
      case (None, None) => {
        getProperty("textStandardExponentRep") // which will SDE!
        Assert.impossible("Above is going to SDE")
      }
      case (Some(tsecStr), None) => {
        SDW(
          WarnID.DeprecatedPropertySeparatorPolicy,
          "Property textStandardExponentCharacter is deprecated. Use textStandardExponentRep instead."
        )
        textStandardExponentCharacterRaw
      }
      case _ =>
        Assert.invariantFailed(
          "combination of textStandardExponentCharacter and textStandardExponentRep not understood"
        )
    }
  }
}

/**
 * By hand because we can set our preference for it via a tunable.
 * And also can require it to be present or not via a tunable.
 */
sealed trait EmptyElementParsePolicy extends EnumValue
object EmptyElementParsePolicy extends Enum[EmptyElementParsePolicy] {
  case object TreatAsMissing extends EmptyElementParsePolicy // deprecated
  case object TreatAsEmpty extends EmptyElementParsePolicy
  case object TreatAsAbsent extends EmptyElementParsePolicy

  override lazy val values =
    Array(TreatAsMissing, TreatAsEmpty, TreatAsAbsent) // deprecated: TreatAsMissing

  def apply(name: String, context: ThrowsSDE): EmptyElementParsePolicy =
    stringToEnum("emptyElementParsePolicy", name, context)
}
trait EmptyElementParsePolicyMixin extends PropertyMixin {

  def tunable: DaffodilTunables

  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for
   * property existence. Just insist on the property you need by
   * using its name. E.g., if you need calendarTimeZone, just use
   * a.calendarTimeZone (where a is an AnnotatedSchemaComponent)
   */
  private def optionEmptyElementPolicyRaw = findPropertyOption("emptyElementParsePolicy")
  private def emptyElementPolicyRaw =
    EmptyElementParsePolicy(requireProperty(optionEmptyElementPolicyRaw).value, this)

  /**
   * Property determines whether Daffodil implements empty elements in a manner consistent with
   * IBM DFDL (as of 2019-05-02), which has policy treatAsAbsent, or implements what is
   * described in the DFDL spec., which is treatAsEmpty - if the syntax of
   * empty (or nullness) is matched, create an empty (or null) item, even if optional, unless
   * the element is entirely absent.
   */
  final lazy val emptyElementParsePolicy = {
    if (this.tunable.requireEmptyElementParsePolicyProperty) {
      emptyElementPolicyRaw // will cause SDE if not found
    } else if (this.optionEmptyElementPolicyRaw.isDefined) {
      emptyElementPolicyRaw // it's there, so just use it.
    } else {
      // prop is not required AND not defined so use tunable value
      // but issue warning (which can be suppressed)
      val defaultEmptyElementParsePolicy = this.tunable.defaultEmptyElementParsePolicy
      SDW(
        WarnID.EmptyElementParsePolicyError,
        "Property 'dfdl:emptyElementParsePolicy' is required but not defined, using tunable '%s' by default.",
        defaultEmptyElementParsePolicy
      )
      defaultEmptyElementParsePolicy
    }
  }

}
