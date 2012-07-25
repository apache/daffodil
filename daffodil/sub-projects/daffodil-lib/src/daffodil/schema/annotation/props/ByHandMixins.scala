package daffodil.schema.annotation.props

// Copyright (C) 2012 Michael J. Beckerle. All Rights Reserved.

import daffodil.exceptions._
import daffodil.dsom.OOLAG.OOLAGHost
import daffodil.dsom.EntityReplacer
import daffodil.dsom.ListOfStringValueAsLiteral
import daffodil.dsom.StringValueAsLiteral

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

  def apply(str : String, self : ThrowsSDE) : Any = { // any because it can be an Int, or "implicit"
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
  def apply(str : String) : String = str
}

object TextNumberBase {
  def apply(str : String, self : ThrowsSDE) : Int = {
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

  def apply(name : String, self : ThrowsSDE) : SeparatorSuppressionPolicy = stringToEnum("separatorSuppressionPolicy", name, self)
}

trait SeparatorSuppressionPolicyMixin extends PropertyMixin { self : OOLAGHost =>

  lazy val separatorSuppressionPolicy = separatorSuppressionPolicy_.value
  private lazy val separatorSuppressionPolicy_ = LV {
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
    
    this.schemaDefinition(raw.length() > 0, "textStandardZeroRep cannot be empty!")
    this.schemaDefinition(!raw.contains("%NL;"), "textStandardZeroRep cannot contain %NL;!")
    this.schemaDefinition(!raw.contains("%ES;"), "textStandardZeroRep cannot contain %ES;!")
    this.schemaDefinition(!raw.contains("%WSP;"), "textStandardZeroRep cannot contain %WSP;!")
    this.schemaDefinition(!raw.contains("%WSP+;"), "textStandardZeroRep cannot contain %WSP+;!")
    this.schemaDefinition(!raw.contains("%WSP*;"), "textStandardZeroRep cannot contain %WSP*;!")
    //TODO: Cannot contain raw bytes
    
    val l = new StringValueAsLiteral(raw,this)
    l.cooked
  }
  
  lazy val textStandardNaNRep = {
    val raw = getProperty("textStandardNaNRep")
    
    this.schemaDefinition(raw.length() > 0, "textStandardZeroRep cannot be empty!")
    this.schemaDefinition(!raw.contains("%NL;"), "textStandardZeroRep cannot contain %NL;!")
    this.schemaDefinition(!raw.contains("%ES;"), "textStandardZeroRep cannot contain %ES;!")
    this.schemaDefinition(!raw.contains("%WSP;"), "textStandardZeroRep cannot contain %WSP;!")
    this.schemaDefinition(!raw.contains("%WSP+;"), "textStandardZeroRep cannot contain %WSP+;!")
    this.schemaDefinition(!raw.contains("%WSP*;"), "textStandardZeroRep cannot contain %WSP*;!")
    //TODO: Cannot contain raw bytes
    
    val l = new StringValueAsLiteral(raw,this)
    l.cooked
  }
  lazy val textStandardZeroRep = {
    val raw = getProperty("textStandardZeroRep")
    
    // Literal Empty String allowed!
    this.schemaDefinition(!raw.contains("%NL;"), "textStandardZeroRep cannot contain %NL;!")
    this.schemaDefinition(!raw.contains("%ES;"), "textStandardZeroRep cannot contain %ES;!")
    //TODO: Cannot contain raw bytes
    
    val l = new ListOfStringValueAsLiteral(raw, this)
    l.cooked
  }
}


