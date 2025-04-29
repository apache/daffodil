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

package org.apache.daffodil.core.grammar.primitives

import org.apache.daffodil.core.dsom.ElementBase
import org.apache.daffodil.core.grammar.Gram
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.lib.schema.annotation.props.gen.Representation
import org.apache.daffodil.lib.schema.annotation.props.gen.TextBooleanJustification
import org.apache.daffodil.lib.schema.annotation.props.gen.TextCalendarJustification
import org.apache.daffodil.lib.schema.annotation.props.gen.TextNumberJustification
import org.apache.daffodil.lib.schema.annotation.props.gen.TextPadKind
import org.apache.daffodil.lib.schema.annotation.props.gen.TextStringJustification
import org.apache.daffodil.lib.schema.annotation.props.gen.TextTrimKind
import org.apache.daffodil.lib.schema.annotation.props.gen.YesNo
import org.apache.daffodil.lib.util.MaybeChar
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType
import org.apache.daffodil.runtime1.processors.TextJustificationType
import org.apache.daffodil.runtime1.processors.TextTruncationType

trait PaddingInfoMixin {
  protected def eBase: ElementBase

  /**
   * parsingPadChar is the pad character for parsing
   * unparsingPadChar is the pad character for unparsing
   *  These are always carried as MaybeChar.
   *
   * We need both, because in the same schema you can have textPadKind="padChar" but
   * textTrimKind="none", so there can't be just one pad char object if it is to
   * carry information about both whether or not a pad character is to be used, and the value.
   */
  lazy val (parsingPadChar, justificationTrim): (MaybeChar, TextJustificationType.Type) =
    eBase.textTrimKind match {
      case TextTrimKind.None => (MaybeChar.Nope, TextJustificationType.None)
      case TextTrimKind.PadChar if eBase.isSimpleType => padCharAndJustificationForType
      case TextTrimKind.PadChar =>
        eBase.SDE("padChar textTrimKind not supported for complexType")
    }

  lazy val (unparsingPadChar: MaybeChar, justificationPad) = {
    val tpk = eBase.textPadKind
    val res = tpk match {
      case TextPadKind.None => (MaybeChar.Nope, TextJustificationType.None)
      case TextPadKind.PadChar
          if eBase.isSimpleType &&
            eBase.impliedRepresentation == Representation.Text =>
        padCharAndJustificationForType
      case _ => (MaybeChar.Nope, TextJustificationType.None)
    }
    res
  }

  lazy val stringTruncationType: TextTruncationType.Type = {
    val res =
      if (eBase.primType != PrimType.String) TextTruncationType.None
      else if (eBase.truncateSpecifiedLengthString eq YesNo.No) TextTruncationType.None
      else if (
        (eBase.lengthKind eq LengthKind.Pattern) ||
        (eBase.lengthKind eq LengthKind.Prefixed)
      ) TextTruncationType.None
      else
        eBase.textStringJustification match {
          case TextStringJustification.Left => TextTruncationType.Left
          case TextStringJustification.Right => TextTruncationType.Right
          case TextStringJustification.Center => TextTruncationType.ErrorIfNeeded
        }
    res
  }

  private lazy val padCharAndJustificationForType: (MaybeChar, TextJustificationType.Type) = {
    val theJust = eBase.primType match {
      case PrimType.Int | PrimType.Byte | PrimType.Short | PrimType.Long | PrimType.Integer |
          PrimType.UnsignedInt | PrimType.UnsignedByte | PrimType.UnsignedShort |
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
      case PrimType.AnyURI => {
        (MaybeChar.Nope, TextJustificationType.None)
      }
    }
    theJust
  }

}

trait Padded extends PaddingInfoMixin { self: Gram =>
  override final protected def eBase = self.context.asInstanceOf[ElementBase]

}
