package daffodil.schema.annotation.props

import daffodil.exceptions._

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
  
  def apply(str : String) : Any = { // any because it can be an Int, or "implicit"
    if (str == "implicit") return Implicit
    val i =
      try {
        str.toInt
      } catch {
        case e => Assert.schemaDefinitionError("For property 'alignment', value must be 'implicit' or an integer. Found: " + str)
      }
    if (allowedAlignmentValues.contains(i)) {
//     val au = alignmentUnits
//     au match {
//       case AlignmentUnits.Bits => i // TODO: implement units * Units.bits
//       case AlignmentUnits.Bytes => i // * Units.bytes
//     }
      i
    }
    else Assert.schemaDefinitionError("For property 'alignment', value must be a power of 2 (and fit in a 32 bit integer). Found: " + str)
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
  def apply(str : String) : Int = {
    str match {
      case "2" => 2
      case "8" => 8
      case "10" => 10
      case "16" => 16
      case _ => Assert.schemaDefinitionError("Illegal number base: " + str) // validation will have checked. So this shoudn't happen.
    }
  }
  
}


sealed trait SeparatorSuppressionPolicy extends SeparatorSuppressionPolicy.Value
object SeparatorSuppressionPolicy extends Enum[SeparatorSuppressionPolicy] {
  case object Never extends SeparatorSuppressionPolicy ; forceConstruction(Never)
  case object TrailingLax extends SeparatorSuppressionPolicy ; forceConstruction(TrailingLax)
  case object Trailing extends SeparatorSuppressionPolicy ; forceConstruction(Trailing)
  case object Always extends SeparatorSuppressionPolicy ; forceConstruction(Always)

  def apply(name: String) : SeparatorSuppressionPolicy = stringToEnum("separatorSuppressionPolicy", name)
}
  
trait SeparatorSuppressionPolicyMixin extends PropertyMixin {
  lazy val separatorSuppressionPolicy = SeparatorSuppressionPolicy(getProperty("separatorSuppressionPolicy"))
}


