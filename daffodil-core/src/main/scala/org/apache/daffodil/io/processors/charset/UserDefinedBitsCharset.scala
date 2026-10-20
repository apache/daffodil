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

package org.apache.daffodil.io.processors.charset

import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder

/**
 * A BitsCharset defined directly in a DFDL schema via a dfdlx:defineCharset
 * statement (see DFDLDefineCharset).
 *
 * Unlike other BitsCharsets, these are NOT registered in the global
 * BitsCharsetDefinitionRegistry (which is populated via the ServiceLoader
 * mechanism). Instead, they are resolved from a per-SchemaSet map of
 * user-defined charsets when the dfdl:encoding property is resolved. That
 * map is built from the dfdlx:defineCharset statements in the schema set,
 * and is carried by the CharsetEv evaluatable, so it survives PDI
 * serialization.
 *
 * All the decode/encode behavior comes from BitsCharsetNonByteSize, which is
 * exactly the same mechanism used by the hardcoded non-byte-size charsets
 * (e.g. X-DFDL-6-BIT-DFI-264-DUI-001).
 *
 * Note that validation of user-provided values (name collisions, decodeString
 * length, character ranges, etc.) happens when the dfdlx:defineCharset
 * statement is compiled (as SDEs), not in this constructor. The constructor
 * usage checks here are just invariants guarding against programming errors.
 */
final class UserDefinedBitsCharset(
  override val name: String,
  override val bitWidthOfACodeUnit: Int,
  override val decodeString: String,
  override val replacementCharCode: Int,
  override val requiredBitOrder: BitOrder,
  override val mandatoryBitAlignment: Int
) extends BitsCharsetNonByteSize {

  /**
   * The default BitsCharset serialization approach writes a proxy that
   * re-resolves the charset by name from the global registry when reading.
   * That cannot possibly work for user-defined charsets since they are not in
   * the registry, so we serialize the entire charset definition inline via a
   * dedicated proxy.
   */
  @throws(classOf[java.io.ObjectStreamException])
  override protected def writeReplace(): Object =
    UserDefinedBitsCharsetProxy(
      name,
      bitWidthOfACodeUnit,
      decodeString,
      replacementCharCode,
      requiredBitOrder.toString,
      mandatoryBitAlignment
    )
}

/**
 * Serialization proxy for UserDefinedBitsCharset. Holds only plain serializable
 * data (the BitOrder enum value is stored as its string name, reconstructed via
 * the BitOrder enum's values, to avoid any dependency on the serialization
 * behavior of generated case objects).
 */
final case class UserDefinedBitsCharsetProxy(
  name: String,
  bitWidthOfACodeUnit: Int,
  decodeString: String,
  replacementCharCode: Int,
  requiredBitOrderName: String,
  mandatoryBitAlignment: Int
) extends java.io.Serializable {

  @throws(classOf[java.io.ObjectStreamException])
  def readResolve(): Object = {
    val bitOrder =
      BitOrder.values.find(_.toString == requiredBitOrderName).getOrElse {
        throw new java.io.InvalidObjectException(
          "Unrecognized bit order '%s' for user defined charset '%s'".format(
            requiredBitOrderName,
            name
          )
        )
      }
    new UserDefinedBitsCharset(
      name,
      bitWidthOfACodeUnit,
      decodeString,
      replacementCharCode,
      bitOrder,
      mandatoryBitAlignment
    )
  }
}
