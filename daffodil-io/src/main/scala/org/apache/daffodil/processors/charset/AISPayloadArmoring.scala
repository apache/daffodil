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
package org.apache.daffodil.processors.charset

import org.apache.daffodil.schema.annotation.props.gen.BitOrder

/**
 * Special purpose. This is not used for decoding anything.
 * The encoder is used to convert strings using the characters
 * allowed, into binary data using the AIS Payload Armoring
 * described here:
 *
 * http://catb.org/gpsd/AIVDM.html#_aivdm_aivdo_payload_armoring
 *
 * To convert a string of length N bytes, You will get 6N bits.
 *
 * The decoder can be used for unit testing, but the point of this class
 * is to make the encoder available for use in un-doing the AIS Payload
 * armoring when parsing, and performing this armoring when unparsing.
 *
 * When encoding from 8-bit say, ascii, or iso-8859-1, this can only encode
 * things that stay within the 64 allowed characters.
 * dfdl:encodingErrorPolicy='error' would check this (once implemented), otherwise
 * where this is used the checking needs to be done separately somehow.
 */
object BitsCharsetAISPayloadArmoring extends {
  override val name = "X-DAFFODIL-AIS-PAYLOAD-ARMORING"
  override val bitWidthOfACodeUnit = 6
  override val decodeString = """0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVW'abcdefghijklmnopqrstuvw"""
  override val replacementCharCode = 0x30
  override val requiredBitOrder = BitOrder.MostSignificantBitFirst
} with BitsCharsetNonByteSize

final class BitsCharsetAISPayloadArmoringDefinition
  extends BitsCharsetDefinition(BitsCharsetAISPayloadArmoring)
