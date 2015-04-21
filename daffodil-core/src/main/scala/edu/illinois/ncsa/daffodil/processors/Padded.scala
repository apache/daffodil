package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.{ AlignmentUnits, LengthKind, TextTrimKind, TextNumberJustification, TextStringJustification, TextCalendarJustification, TextBooleanJustification }
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.parsers.LeadingSkipRegionParser
import edu.illinois.ncsa.daffodil.processors.parsers.TrailingSkipRegionParser
import edu.illinois.ncsa.daffodil.processors.parsers.AlignmentFillParser
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextPadKind

trait Padded { self: Terminal =>
  val eBase = self.context.asInstanceOf[ElementBase]
  /**
   * parsingPadChar is the pad character for parsing
   * unparsingPadChar is the pad character for unparsing
   *  These are always carried as Maybe[Char].
   *
   * We need both, because in the same schema you can have textPadKind="padChar" but
   * textTrimKind="none", so there can't be just one pad char object if it is to
   * carry information about both whether or not a pad character is to be used, and the value.
   */
  lazy val (parsingPadChar, justificationTrim): (Maybe[Char], TextJustificationType.Type) = eBase.textTrimKind match {
    case TextTrimKind.None => (Nope, TextJustificationType.None)
    case TextTrimKind.PadChar if eBase.isSimpleType => padCharAndJustificationForType
  }

  lazy val (unparsingPadChar, justificationPad) = eBase.textPadKind match {
    case TextPadKind.None => (Nope, TextJustificationType.None)
    case TextPadKind.PadChar if eBase.isSimpleType => padCharAndJustificationForType
  }

  private lazy val padCharAndJustificationForType: (Maybe[Char], TextJustificationType.Type) = {
    val theJust = eBase.primType match {
      case PrimType.Int | PrimType.Byte | PrimType.Short | PrimType.Long |
        PrimType.Integer | PrimType.UnsignedInt | PrimType.UnsignedByte | PrimType.UnsignedShort |
        PrimType.UnsignedLong | PrimType.Double | PrimType.Float | PrimType.Decimal |
        PrimType.NonNegativeInteger => {
        val padChar = eBase.textNumberPadCharacter.charAt(0)
        val just = eBase.textNumberJustification match {
          case TextNumberJustification.Left => TextJustificationType.Left
          case TextNumberJustification.Right => TextJustificationType.Right
          case TextNumberJustification.Center => TextJustificationType.Center
        }
        (One(padChar), just)
      }
      case PrimType.String => {
        val padChar = eBase.textStringPadCharacter.charAt(0)
        val just = eBase.textStringJustification match {
          case TextStringJustification.Left => TextJustificationType.Left
          case TextStringJustification.Right => TextJustificationType.Right
          case TextStringJustification.Center => TextJustificationType.Center
        }
        (One(padChar), just)
      }
      case PrimType.DateTime | PrimType.Date | PrimType.Time => {
        val padChar = eBase.textCalendarPadCharacter.charAt(0)
        val just = eBase.textCalendarJustification match {
          case TextCalendarJustification.Left => TextJustificationType.Left
          case TextCalendarJustification.Right => TextJustificationType.Right
          case TextCalendarJustification.Center => TextJustificationType.Center
        }
        (One(padChar), just)
      }
      case PrimType.Boolean => {
        val padChar = eBase.textBooleanPadCharacter.charAt(0)
        val just = eBase.textBooleanJustification match {
          case TextBooleanJustification.Left => TextJustificationType.Left
          case TextBooleanJustification.Right => TextJustificationType.Right
          case TextBooleanJustification.Center => TextJustificationType.Center
        }
        (One(padChar), just)
      }
      case PrimType.HexBinary => {
        (Nope, TextJustificationType.None)
      }
    }
    theJust
  }

}
