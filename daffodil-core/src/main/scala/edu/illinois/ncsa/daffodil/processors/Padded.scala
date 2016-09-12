/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextBooleanJustification
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextCalendarJustification
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextNumberJustification
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextPadKind
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextStringJustification
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextTrimKind
import edu.illinois.ncsa.daffodil.util.MaybeChar
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.YesNo
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.Representation
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthKind

trait PaddingInfoMixin {
  def eBase: ElementBase

  /**
   * parsingPadChar is the pad character for parsing
   * unparsingPadChar is the pad character for unparsing
   *  These are always carried as MaybeChar.
   *
   * We need both, because in the same schema you can have textPadKind="padChar" but
   * textTrimKind="none", so there can't be just one pad char object if it is to
   * carry information about both whether or not a pad character is to be used, and the value.
   */
  lazy val (parsingPadChar, justificationTrim): (MaybeChar, TextJustificationType.Type) = eBase.textTrimKind match {
    case TextTrimKind.None => (MaybeChar.Nope, TextJustificationType.None)
    case TextTrimKind.PadChar if eBase.isSimpleType => padCharAndJustificationForType
  }

  lazy val (unparsingPadChar: MaybeChar, justificationPad) = {
    val tpk = eBase.textPadKind
    val res = tpk match {
      case TextPadKind.None => (MaybeChar.Nope, TextJustificationType.None)
      case TextPadKind.PadChar if eBase.isSimpleType &&
        eBase.impliedRepresentation == Representation.Text => padCharAndJustificationForType
      case _ => (MaybeChar.Nope, TextJustificationType.None)
    }
    res
  }

  lazy val stringTruncationType: TextTruncationType.Type = {
    val res =
      if (eBase.primType != PrimType.String) TextTruncationType.None
      else if (eBase.truncateSpecifiedLengthString eq YesNo.No) TextTruncationType.None
      else if ((eBase.lengthKind eq LengthKind.Pattern) ||
        (eBase.lengthKind eq LengthKind.Prefixed)) TextTruncationType.None
      else eBase.textStringJustification match {
        case TextStringJustification.Left => TextTruncationType.Left
        case TextStringJustification.Right => TextTruncationType.Right
        case TextStringJustification.Center => {
          eBase.SDE("Properties dfdl:truncateSpecifiedLengthString 'yes' and dfdl:textStringJustification 'center' are incompatible.")
        }
      }
    res
  }

  private lazy val padCharAndJustificationForType: (MaybeChar, TextJustificationType.Type) = {
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
        (MaybeChar(padChar), just)
      }
      case PrimType.String => {
        val padChar = eBase.textStringPadCharacter.charAt(0)
        val just = eBase.textStringJustification match {
          case TextStringJustification.Left => TextJustificationType.Left
          case TextStringJustification.Right => TextJustificationType.Right
          case TextStringJustification.Center => TextJustificationType.Center
        }
        (MaybeChar(padChar), just)
      }
      case PrimType.DateTime | PrimType.Date | PrimType.Time => {
        val padChar = eBase.textCalendarPadCharacter.charAt(0)
        val just = eBase.textCalendarJustification match {
          case TextCalendarJustification.Left => TextJustificationType.Left
          case TextCalendarJustification.Right => TextJustificationType.Right
          case TextCalendarJustification.Center => TextJustificationType.Center
        }
        (MaybeChar(padChar), just)
      }
      case PrimType.Boolean => {
        val padChar = eBase.textBooleanPadCharacter.charAt(0)
        val just = eBase.textBooleanJustification match {
          case TextBooleanJustification.Left => TextJustificationType.Left
          case TextBooleanJustification.Right => TextJustificationType.Right
          case TextBooleanJustification.Center => TextJustificationType.Center
        }
        (MaybeChar(padChar), just)
      }
      case PrimType.HexBinary => {
        (MaybeChar.Nope, TextJustificationType.None)
      }
    }
    theJust
  }

}

trait Padded extends PaddingInfoMixin { self: Gram =>
  override final def eBase = self.context.asInstanceOf[ElementBase]

}
