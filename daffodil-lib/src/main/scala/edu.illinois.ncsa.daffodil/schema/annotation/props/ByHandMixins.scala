package edu.illinois.ncsa.daffodil.schema.annotation.props

// Copyright (C) 2012 Michael J. Beckerle. All Rights Reserved.

import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.dsom.OOLAG.OOLAGHost
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
        case e => self.schemaDefinitionError("For property 'alignment', value must be 'implicit' or an integer. Found: %s", str)
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

object FillByteType {
  def apply(str: String): String = str
}

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
  case object TrailingLax extends SeparatorSuppressionPolicy; forceConstruction(TrailingLax)
  case object Trailing extends SeparatorSuppressionPolicy; forceConstruction(Trailing)
  case object Always extends SeparatorSuppressionPolicy; forceConstruction(Always)

  def apply(name: String, self: ThrowsSDE): SeparatorSuppressionPolicy = stringToEnum("separatorSuppressionPolicy", name, self)
}

trait SeparatorSuppressionPolicyMixin
  extends PropertyMixin { self: OOLAGHost =>

  lazy val separatorSuppressionPolicy = separatorSuppressionPolicy_.value
  private lazy val separatorSuppressionPolicy_ = LV('separatorSuppressionPolicy) {
    val sp = getPropertyOption("separatorPolicy")
    val ssp = getPropertyOption("separatorSuppressionPolicy")
    ssp match {
      case Some(sspStr) => SeparatorSuppressionPolicy(sspStr, this)
      case None => {
        sp match {
          case Some("required") => SeparatorSuppressionPolicy.Never
          case Some("suppressed") => SeparatorSuppressionPolicy.Always
          case Some("suppressedAtEndStrict") => SeparatorSuppressionPolicy.Trailing
          case Some("suppressedAtEndLax") => SeparatorSuppressionPolicy.TrailingLax
          case None => {
            getProperty("separatorPolicy") // which will fail!
            Assert.impossible()
          }
          case Some(other) => Assert.impossibleCase()
        }
      }
    }
  }
}

trait TextNumberFormatMixin extends PropertyMixin {
  lazy val textStandardInfinityRep = {
    val raw = getProperty("textStandardInfinityRep")

    this.schemaDefinition(raw.length() > 0, "textStandardInfinityRep cannot be empty!")
    this.schemaDefinition(!raw.contains("%NL;"), "textStandardInfinityRep cannot contain NL!")
    this.schemaDefinition(!raw.contains("%ES;"), "textStandardInfinityRep cannot contain ES!")
    this.schemaDefinition(!raw.contains("%WSP;"), "textStandardInfinityRep cannot contain WSP!")
    this.schemaDefinition(!raw.contains("%WSP+;"), "textStandardInfinityRep cannot contain WSP+!")
    this.schemaDefinition(!raw.contains("%WSP*;"), "textStandardInfinityRep cannot contain WSP*!")
    // TODO: Cannot contain raw bytes

    val l = new StringValueAsLiteral(raw, this)
    l.cooked
  }

  lazy val textStandardNaNRep = {
    val raw = getProperty("textStandardNaNRep")

    this.schemaDefinition(raw.length() > 0, "textStandardNaNRep cannot be empty!")
    this.schemaDefinition(!raw.contains("%NL;"), "textStandardNaNRep cannot contain NL!")
    this.schemaDefinition(!raw.contains("%ES;"), "textStandardNaNRep cannot contain ES!")
    this.schemaDefinition(!raw.contains("%WSP;"), "textStandardNaNRep cannot contain WSP!")
    this.schemaDefinition(!raw.contains("%WSP+;"), "textStandardNaNRep cannot contain WSP+!")
    this.schemaDefinition(!raw.contains("%WSP*;"), "textStandardNaNRep cannot contain WSP*!")
    // TODO: Cannot contain raw bytes

    val l = new StringValueAsLiteral(raw, this)
    l.cooked
  }
  lazy val textStandardZeroRep = {
    val raw = getProperty("textStandardZeroRep")

    // Literal Empty String allowed!
    this.schemaDefinition(!raw.contains("%NL;"), "textStandardZeroRep cannot contain NL!")
    this.schemaDefinition(!raw.contains("%ES;"), "textStandardZeroRep cannot contain ES!")
    // TODO: Cannot contain raw bytes

    val l = new ListOfStringValueAsLiteral(raw, this)
    l.cooked
  }
}

trait StringTextMixin extends PropertyMixin
  with LengthUnitsMixin {
  lazy val textStringPadCharacterRaw = getProperty("textStringPadCharacter")
  lazy val textStringPadCharacter = {
    val raw = textStringPadCharacterRaw

    // Can be a literal character or DFDL entity

    // DFDL Character classes are not allowed
    this.schemaDefinition(!(raw.length() == 0), "textStringPadCharacter cannot be empty!")
    this.schemaDefinition(!raw.contains("%NL;"), "textStringPadCharacter cannot contain NL!")
    this.schemaDefinition(!raw.contains("%ES;"), "textStringPadCharacter cannot contain ES!")
    this.schemaDefinition(!raw.contains("%WSP;"), "textStringPadCharacter cannot contain WSP!")
    this.schemaDefinition(!raw.contains("%WSP+;"), "textStringPadCharacter cannot contain WSP+!")
    this.schemaDefinition(!raw.contains("%WSP*;"), "textStringPadCharacter cannot contain WSP*!")

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
    this.schemaDefinition(!(raw.length() == 0), "textNumberPadCharacter cannot be empty!")
    this.schemaDefinition(!raw.contains("%NL;"), "textNumberPadCharacter cannot contain NL!")
    this.schemaDefinition(!raw.contains("%ES;"), "textNumberPadCharacter cannot contain ES!")
    this.schemaDefinition(!raw.contains("%WSP;"), "textNumberPadCharacter cannot contain WSP!")
    this.schemaDefinition(!raw.contains("%WSP+;"), "textNumberPadCharacter cannot contain WSP+!")
    this.schemaDefinition(!raw.contains("%WSP*;"), "textNumberPadCharacter cannot contain WSP*!")

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
    this.schemaDefinition(!(raw.length() == 0), "textBooleanPadCharacter cannot be empty!")
    this.schemaDefinition(!raw.contains("%NL;"), "textBooleanPadCharacter cannot contain NL!")
    this.schemaDefinition(!raw.contains("%ES;"), "textBooleanPadCharacter cannot contain ES!")
    this.schemaDefinition(!raw.contains("%WSP;"), "textBooleanPadCharacter cannot contain WSP!")
    this.schemaDefinition(!raw.contains("%WSP+;"), "textBooleanPadCharacter cannot contain WSP+!")
    this.schemaDefinition(!raw.contains("%WSP*;"), "textBooleanPadCharacter cannot contain WSP*!")

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
    this.schemaDefinition(!(raw.length() == 0), "textCalendarPadCharacter cannot be empty!")
    this.schemaDefinition(!raw.contains("%NL;"), "textCalendarPadCharacter cannot contain NL!")
    this.schemaDefinition(!raw.contains("%ES;"), "textCalendarPadCharacter cannot contain ES!")
    this.schemaDefinition(!raw.contains("%WSP;"), "textCalendarPadCharacter cannot contain WSP!")
    this.schemaDefinition(!raw.contains("%WSP+;"), "textCalendarPadCharacter cannot contain WSP+!")
    this.schemaDefinition(!raw.contains("%WSP*;"), "textCalendarPadCharacter cannot contain WSP*!")

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

trait NillableMixin extends PropertyMixin
  with NilKindMixin
  with NilValueDelimiterPolicyMixin
  with RepresentationMixin {

  // Information regarding Representation was needed here to characterize
  // appropriate behavior of nilValue
  lazy val nilValue = {
    val raw = getProperty("nilValue")

    this.schemaDefinition(raw.length() > 0, "nilValue cannot be empty!")

    // Commented out replacement because the replacement takes place within
    // the Parser of StaticText/DynamicText
    this.nilKind match {
      case NilKind.LiteralCharacter => {
        this.schemaDefinition(!raw.contains("%NL;"), "nilValue cannot contain NL!")
        this.schemaDefinition(!raw.contains("%ES;"), "nilValue cannot contain ES!")
        this.schemaDefinition(!raw.contains("%WSP;"), "nilValue cannot contain WSP!")
        this.schemaDefinition(!raw.contains("%WSP+;"), "nilValue cannot contain WSP+!")
        this.schemaDefinition(!raw.contains("%WSP*;"), "nilValue cannot contain WSP*!")
        //        val l = new SingleCharacterLiteral(raw, this)
        //        List(l.cooked)
      }
      case NilKind.LogicalValue => {
        this.schemaDefinition(!raw.contains("%NL;"), "nilValue cannot contain NL!")
        this.schemaDefinition(!raw.contains("%ES;"), "nilValue cannot contain ES!")
        this.schemaDefinition(!raw.contains("%WSP;"), "nilValue cannot contain WSP!")
        this.schemaDefinition(!raw.contains("%WSP+;"), "nilValue cannot contain WSP+!")
        this.schemaDefinition(!raw.contains("%WSP*;"), "nilValue cannot contain WSP*!")
        //        val l = new ListOfStringValueAsLiteral(raw, this)
        //        l.cooked
      }
      case NilKind.LiteralValue => {
        this.representation match {
          case Representation.Binary => {
            this.schemaDefinition(!raw.contains("%NL;"), "nilValue can only contain ES!")
            this.schemaDefinition(!raw.contains("%WSP;"), "nilValue can only contain ES!")
            this.schemaDefinition(!raw.contains("%WSP+;"), "nilValue can only contain ES!")
            this.schemaDefinition(!raw.contains("%WSP*;"), "nilValue can only contain ES!")
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

