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

import java.util.regex.Pattern
import scala.xml.Node

import org.apache.daffodil.io.processors.charset.BitsCharset
import org.apache.daffodil.io.processors.charset.CharsetUtils
import org.apache.daffodil.io.processors.charset.UserDefinedBitsCharset
import org.apache.daffodil.lib.cookers.EntityReplacer
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.xml.XMLUtils

object DFDLDefineCharset {
  def apply(node: Node, sd: SchemaDocument) = {
    val dc = new DFDLDefineCharset(node, sd)
    dc.initialize()
    dc
  }

  private final val MaxSupportedBitWidth = 8

  /**
   * The replacement character, used in a decodeString to mark an unassigned
   * (unmappable) code point. Must match BitsCharsetNonByteSize.
   */
  private final val UnicodeReplacementCharacter = '\uFFFD'

  /*
   * Character class entities (NL, ES, WSP, WSP*, WSP+) are not DFDL character
   * entities (they are whitespace-classing entities used for delimiter/pattern
   * properties) and have no meaning in a decodeString, so they are rejected.
   * The regex matches them only when not escaped by a preceding % or by %%
   * (e.g. %%WSP*; is a literal percent followed by WSP; text, not an entity).
   */
  private lazy val disallowedCharClassRegex = {
    "(?:^|[^%])(?:%%)*(%(?:NL|ES|WSP|WSP\\+|WSP\\*);)".r
  }
}

/**
 * Represents a dfdlx:defineCharset statement, which defines a named,
 * fixed-width (1-8 bits per code unit) character set directly in the DFDL
 * schema, usable by name via the dfdl:encoding property.
 *
 * Example:
 * {{{
 * <xs:annotation>
 *   <xs:appinfo source="http://www.ogf.org/dfdl/">
 *     <dfdlx:defineCharset
 *         name="BitsCharset6BitDFI408DUI505"
 *         bitWidthOfACodeUnit="6"
 *         decodeString="%#x20;123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ..."
 *         replacementCharCode="0x0"
 *         requiredBitOrder="leastSignificantBitFirst"/>
 *   </xs:appinfo>
 * </xs:annotation>
 * }}}
 *
 * The decodeString supports DFDL Character Entities (per the DFDL
 * specification), e.g. %NUL;, %NBSP;, %#xHH;, %#DDD;, and %#rHH;, in addition
 * to ordinary (XML-thawed) characters. The cooked decodeString must contain
 * exactly 2**bitWidthOfACodeUnit characters, each with a Unicode code point
 * less than 256. The U+FFFD (replacement character) marks an unassigned
 * (unmappable) code point.
 */
final class DFDLDefineCharset(node: Node, sd: SchemaDocument)
  extends DFDLDefiningAnnotation(node, sd) {

  import DFDLDefineCharset.*

  requiredEvaluationsIfActivated(bitsCharset)

  private lazy val bitWidthOfACodeUnit: Int = {
    val str = getAttributeRequired("bitWidthOfACodeUnit")
    val i = str.toIntOption
    if (!i.isDefined)
      SDE(
        "The bitWidthOfACodeUnit attribute must be an integer, but was '%s'.",
        str
      )
    schemaDefinitionUnless(
      i.get >= 1 && i.get <= MaxSupportedBitWidth,
      "The bitWidthOfACodeUnit attribute must be between 1 and %d, but was %d.",
      MaxSupportedBitWidth,
      i.get
    )
    i.get
  }

  /**
   * Replace DFDL character entities (per DFDL spec section 12.3.1) found in
   * the decodeString with the characters they represent. The replacement is
   * performed as a single left-to-right pass so that escaped percents (%%)
   * and entities representing the percent character (%#x25;) are handled
   * unambiguously, which the multi-pass EntityReplacer.replaceAll does not
   * guarantee.
   */
  private def cookEntities(thawed: String): String = EntityReplacer { er =>
    val namedEntities = er.entityCharacterUnicode.map { case (name, value, _) =>
      ("%" + name + ";", value)
    }.toMap
    val sb = new StringBuilder
    var rest = thawed
    while (rest.nonEmpty) {
      if (rest.startsWith("%%")) {
        sb += '%'
        rest = rest.substring(2)
      } else {
        def codePointToChar(v: Int, entity: String): Char = {
          schemaDefinitionUnless(
            v <= 0xffff,
            "The decodeString attribute of dfdlx:defineCharset contains the entity %s with code point %d, which is larger than a Unicode code unit (U+FFFF).",
            entity,
            v
          )
          v.toChar
        }
        val (replaced, remaining) = rest match {
          case er.hexRegex(entity, r) =>
            val v = Integer.parseInt(entity.substring(3, entity.length - 1), 16)
            (codePointToChar(v, entity).toString, r)
          case er.decRegex(entity, r) =>
            val v = Integer.parseInt(entity.substring(2, entity.length - 1), 10)
            (codePointToChar(v, entity).toString, r)
          case er.byteRegex(entity, r) =>
            val v = Integer.parseInt(entity.substring(3, entity.length - 2), 16)
            (v.toChar.toString, r)
          case er.charEntityRegex(entity, r) =>
            (namedEntities(entity), r)
          case _ =>
            if (rest.startsWith("%"))
              SDE(
                "Invalid DFDL entity found in the decodeString attribute of dfdlx:defineCharset, at '%s', in string: %s",
                rest.substring(0, math.min(rest.length, 10)),
                thawed
              )
            (rest.substring(0, 1), rest.substring(1))
        }
        sb ++= replaced
        rest = remaining
      }
    }
    sb.toString
  }

  /**
   * Cook the raw decodeString attribute value: reject character class
   * entities, thaw XML entities, then replace DFDL character entities.
   */
  private lazy val decodeString: String = {
    val raw = getAttributeRequired("decodeString")
    val disallowed = disallowedCharClassRegex.findAllMatchIn(raw).map { _.group(1) }
    schemaDefinitionUnless(
      disallowed.isEmpty,
      "The decodeString attribute of dfdlx:defineCharset contains disallowed character class entity(s): %s",
      disallowed.mkString(", ")
    )
    val thawed = XMLUtils.unescape(raw)
    val cooked = cookEntities(thawed)
    schemaDefinitionUnless(
      cooked.length == (1 << bitWidthOfACodeUnit),
      "The decodeString attribute of dfdlx:defineCharset must contain exactly %d characters (2 ** bitWidthOfACodeUnit = 2 ** %d), but after DFDL entity replacement it contained %d characters.",
      1 << bitWidthOfACodeUnit,
      bitWidthOfACodeUnit,
      cooked.length
    )
    cooked.foreach { c =>
      schemaDefinitionUnless(
        c < 256 || c == UnicodeReplacementCharacter,
        "The decodeString attribute of dfdlx:defineCharset may only contain characters with Unicode code points less than 256 (the replacement character U+FFFD is also allowed, to mark an unmapped code point), but found character U+%04X.",
        c.toInt
      )
    }
    cooked
  }

  private lazy val replacementCharCode: Int = {
    val str = getAttributeRequired("replacementCharCode")
    val opt =
      if (Pattern.matches("[+-]?0[xX][0-9a-fA-F]+", str))
        Some(Integer.parseInt(str.replaceFirst("[+-]?0[xX]", ""), 16))
      else if (Pattern.matches("[+-]?[0-9]+", str))
        Some(Integer.parseInt(str, 10))
      else None
    if (!opt.isDefined)
      SDE(
        "The replacementCharCode attribute must be an integer (decimal or 0x hexadecimal), but was '%s'.",
        str
      )
    val i = opt.get
    schemaDefinitionUnless(
      i >= 0 && i < (1 << bitWidthOfACodeUnit),
      "The replacementCharCode attribute must be between 0 and %d (inclusive) for a bitWidthOfACodeUnit of %d, but was %d.",
      (1 << bitWidthOfACodeUnit) - 1,
      bitWidthOfACodeUnit,
      i
    )
    i
  }

  private lazy val requiredBitOrder: BitOrder = {
    val str = getAttributeOption("requiredBitOrder").getOrElse("mostSignificantBitFirst")
    BitOrder(str, this)
  }

  private lazy val mandatoryBitAlignment: Int = {
    val str = getAttributeOption("mandatoryBitAlignment").getOrElse("1")
    val i = str.toIntOption
    if (!i.isDefined)
      SDE(
        "The mandatoryBitAlignment attribute must be an integer, but was '%s'.",
        str
      )
    schemaDefinitionUnless(
      i.get >= 1,
      "The mandatoryBitAlignment attribute must be at least 1, but was %d.",
      i.get
    )
    i.get
  }

  /**
   * The BitsCharset defined by this statement. Demanding this value performs
   * all of the schema definition error checking of the statement's attributes.
   */
  lazy val bitsCharset: BitsCharset = {
    // demand all of the validating attributes
    val bw = bitWidthOfACodeUnit
    val ds = decodeString
    val rc = replacementCharCode
    val bo = requiredBitOrder
    val mba = mandatoryBitAlignment

    // Note: no overriding of built-in (or ServiceLoader plugin) charsets of the
    // same name--the name must not already be a known charset name.
    schemaDefinitionWhen(
      CharsetUtils.getCharset(name) != null,
      "The dfdlx:defineCharset name '%s' is already a defined/registered charset name. Character sets defined by dfdlx:defineCharset must not override predefined charsets.",
      name
    )

    new UserDefinedBitsCharset(name, bw, ds, rc, bo, mba)
  }
}
