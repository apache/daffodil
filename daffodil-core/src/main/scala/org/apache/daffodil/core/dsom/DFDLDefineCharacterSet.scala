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
import org.apache.daffodil.lib.cookers.DefineCharacterSetCodePointsCooker
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder

object DFDLDefineCharacterSet {
  def apply(node: Node, sd: SchemaDocument) = {
    val dc = new DFDLDefineCharacterSet(node, sd)
    dc.initialize()
    dc
  }

  private final val MaxSupportedBitWidth = 8
}

/**
 * Represents a dfdlx:defineCharacterSet statement, which defines a named,
 * fixed-width (1-8 bits per code unit) character set directly in the DFDL
 * schema, usable by name via the dfdl:encoding property.
 *
 * Example:
 * {{{
 * <xs:annotation>
 *   <xs:appinfo source="http://www.ogf.org/dfdl/">
 *     <dfdlx:defineCharacterSet
 *         name="BitsCharset6Bit"
 *         width="6"
 *         codePoints="%#x20;123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ..."
 *         replacement="?"
 *         bitOrder="leastSignificantBitFirst"/>
 *   </xs:appinfo>
 * </xs:annotation>
 * }}}
 *
 * The codePoints supports DFDL Character Entities (per the DFDL
 * specification), e.g. %NUL;, %NBSP;, %#xHH;, %#DDD;, and %#rHH;, in addition
 * to ordinary characters. The cooked codePoints must contain exactly 2**width
 * characters The U+FFFD (replacement character) marks an unassigned
 * (unmappable) code point.
 */
final class DFDLDefineCharacterSet(node: Node, sd: SchemaDocument)
  extends DFDLDefiningAnnotation(node, sd) {

  import DFDLDefineCharacterSet.*

  requiredEvaluationsIfActivated(bitsCharset)

  private lazy val width: Int = {
    val str = getAttributeRequired("width")
    val i = str.toIntOption
    if (!i.isDefined)
      SDE(
        "The width attribute must be an integer, but was '%s'.",
        str
      )
    schemaDefinitionUnless(
      i.get >= 1 && i.get <= MaxSupportedBitWidth,
      "The width attribute must be between 1 and %d, but was %d.",
      MaxSupportedBitWidth,
      i.get
    )
    i.get
  }

  /**
   * Cook the raw codePoints attribute value: reject character class
   * entities, thaw XML entities, then replace DFDL character entities.
   */
  private lazy val codePoints: String = {
    val raw = getAttributeRequired("codePoints")
    val cooked = DefineCharacterSetCodePointsCooker.convertConstant(raw, this, true)
    schemaDefinitionUnless(
      cooked.length == 1 << width,
      "The codePoints attribute of dfdlx:defineCharacterSet must contain exactly %d characters, but after DFDL entity replacement it contained %d characters.",
      1 << width,
      cooked.length
    )
    cooked
  }

  private lazy val replacement: Int = {
    val str = getAttributeRequired("replacement")
    val i = str.toIntOption
    schemaDefinitionUnless(
      (i.isDefined && (i.get >= 0 && i.get < codePoints.length)),
      s"The replacement attribute must be defined as a an integer between 0 and ${codePoints.length - 1}, provided value: $str"
    )
    i.get
  }

  private lazy val bitOrder: BitOrder = {
    val str = getAttributeRequired("bitOrder")
    BitOrder(str, this)
  }

  private lazy val alignment: Int = {
    val str = getAttributeRequired("alignment")
    val i = str.toIntOption
    schemaDefinitionUnless(
      (i.isDefined && (i.get == 1 || i.get == 8)),
      s"The alignment attribute must be set to either 1 or 8 bits, but was $str"
    )
    i.get
  }

  /**
   * The BitsCharset defined by this statement. Demanding this value performs
   * all of the schema definition error checking of the statement's attributes.
   */
  lazy val bitsCharset: BitsCharset = {
    // demand all of the validating attributes
    val bw = width
    val ds = codePoints
    val rc = replacement
    val bo = bitOrder
    val mba = alignment

    // Note: no overriding of built-in (or ServiceLoader plugin) charsets of the
    // same name--the name must not already be a known charset name.
    schemaDefinitionWhen(
      CharsetUtils.getCharset(name) != null,
      "The dfdlx:defineCharacterSet name '%s' is already a defined/registered charset name. Character sets defined by dfdlx:defineCharacterSet must not override predefined charsets.",
      name
    )

    new UserDefinedBitsCharset(name, bw, ds, rc, bo, mba)
  }
}
