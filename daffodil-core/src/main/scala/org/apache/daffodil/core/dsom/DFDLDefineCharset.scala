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
 *         name="BitsCharset6Bit"
 *         bitWidthOfACodeUnit="6"
 *         decodeString="%#x20;123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ..."
 *         replacementChar="?"
 *         bitOrder="leastSignificantBitFirst"/>
 *   </xs:appinfo>
 * </xs:annotation>
 * }}}
 *
 * The decodeString supports DFDL Character Entities (per the DFDL
 * specification), e.g. %NUL;, %NBSP;, %#xHH;, %#DDD;, and %#rHH;, in addition
 * to ordinary (XML-thawed) characters. The cooked decodeString must contain
 * exactly 2**bitWidthOfACodeUnit characters The U+FFFD (replacement character)
 * marks an unassigned (unmappable) code point.
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
   * Cook the raw decodeString attribute value: reject character class
   * entities, thaw XML entities, then replace DFDL character entities.
   */
  private lazy val decodeString: String = {
    val raw = getAttributeRequired("decodeString")
    val thawed = XMLUtils.unescape(raw)
    val cooked = EntityReplacer { _.replaceAll(thawed, None, forUnparse=true) }
    schemaDefinitionUnless(
      cooked.codePointCount(0, cooked.length) == (1 << bitWidthOfACodeUnit),
      "The decodeString attribute of dfdlx:defineCharset must contain exactly %d characters, but after DFDL entity replacement it contained %d characters.",
      1 << bitWidthOfACodeUnit,
      bitWidthOfACodeUnit,
      cooked.codePointCount(0, cooked.length)
    )
    cooked
  }

  private lazy val replacementCharCode: Int = {
    val raw = getAttributeRequired("replacementChar")
    val thawed = XMLUtils.unescape(raw)
    val cooked = EntityReplacer { _.replaceAll(thawed, None) }
    schemaDefinitionUnless(
      (cooked.codePointCount(0, cooked.length) == 1) && decodeString.contains(cooked),
      s"The replacementChar attribute must be defined as a single character found in the provided decodeString. Specified replacementChar: $cooked"
    )
    decodeString.indexOf(cooked)
  }

  private lazy val bitOrder: BitOrder = {
    val str = getAttributeRequired("bitOrder")
    BitOrder(str, this)
  }

  private lazy val alignement: Int = {
    val str = getAttributeRequired("alignment")
    val i = str.toIntOption
    schemaDefinitionUnless(
      (i.isDefined && i.get >= 1),
      s"The alignment attribute must be an integer >= 1, but was $str")
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
    val bo = bitOrder
    val mba = alignement

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
