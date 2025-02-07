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

/*
 * This file contains special character encodings used by various US military data formats
 * The DFI/DUI numbers may be used to lookup the definition of the encoding by those with access
 * to the relevent specification. In cases where multiple DFI/DUI pairs make use of the same encoding,
 * the decision of which pair to name the encoding after is arbitrary.
 *
 * Unless otherwise stated, characters defined as "NO STATEMENT" are assumed to be padding characters.
 * These characters are translated into a no-break space (U+00A0)
 */

package org.apache.daffodil.io.processors.charset

import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder

/**
 * X-DFDL-6-BIT-DFI-264-DUI-001, special 6 bit encoding
 */
object BitsCharset6BitDFI264DUI001 extends BitsCharsetNonByteSize {
  override lazy val name = "X-DFDL-6-BIT-DFI-264-DUI-001"
  override lazy val bitWidthOfACodeUnit = 6
  override lazy val decodeString =
    " 123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD0"
  override lazy val replacementCharCode = 0x0
  override lazy val requiredBitOrder = BitOrder.LeastSignificantBitFirst
}

final class BitsCharset6BitDFI264DUI001Definition
  extends BitsCharsetDefinition(BitsCharset6BitDFI264DUI001)

sealed abstract class BitsCharset6BitDFI311DUI002Base extends BitsCharsetNonByteSize {
  override lazy val bitWidthOfACodeUnit = 6
  override lazy val decodeString =
    "\u00A0ABCDEFGHIJKLMNOPQRSTUVWXYZ\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD \uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD0123456789\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD"
  override lazy val replacementCharCode = 0x0
}

object BitsCharset6BitDFI311DUI002 extends BitsCharset6BitDFI311DUI002Base {
  override lazy val name = "X-DFDL-6-BIT-DFI-311-DUI-002"
  override lazy val requiredBitOrder = BitOrder.LeastSignificantBitFirst
}

final class BitsCharset6BitDFI311DUI002Definition
  extends BitsCharsetDefinition(BitsCharset6BitDFI311DUI002)

object BitsCharset6BitICAOAircraftID extends BitsCharset6BitDFI311DUI002Base {
  override lazy val name = "X-DFDL-6-BIT-ICAO-Aircraft-ID"
  override lazy val requiredBitOrder = BitOrder.MostSignificantBitFirst
}

final class BitsCharset6BitICAOAircraftIDDefinition
  extends BitsCharsetDefinition(BitsCharset6BitICAOAircraftID)

object BitsCharset3BitDFI336DUI001 extends BitsCharsetNonByteSize {
  override lazy val name = "X-DFDL-3-BIT-DFI-336-DUI-001"
  override lazy val bitWidthOfACodeUnit = 3
  override lazy val decodeString = """12345678"""
  override lazy val replacementCharCode = 0x0
  override lazy val requiredBitOrder = BitOrder.LeastSignificantBitFirst
}

final class BitsCharset3BitDFI336DUI001Definition
  extends BitsCharsetDefinition(BitsCharset3BitDFI336DUI001)

object BitsCharset4BitDFI746DUI002 extends BitsCharsetNonByteSize {
  override lazy val name = "X-DFDL-4-BIT-DFI-746-DUI-002"
  override lazy val bitWidthOfACodeUnit = 4
  override lazy val decodeString = """ABCDEFGHIJKLMNPQ"""
  override lazy val replacementCharCode = 0x0
  override lazy val requiredBitOrder = BitOrder.LeastSignificantBitFirst
}

final class BitsCharset4BitDFI746DUI002Definition
  extends BitsCharsetDefinition(BitsCharset4BitDFI746DUI002)

object BitsCharset3BitDFI746DUI002 extends BitsCharsetNonByteSize {
  override lazy val name = "X-DFDL-3-BIT-DFI-746-DUI-002"
  override lazy val bitWidthOfACodeUnit = 3
  override lazy val decodeString = """ABCDEFGH"""
  override lazy val replacementCharCode = 0x0
  override lazy val requiredBitOrder = BitOrder.LeastSignificantBitFirst
}

final class BitsCharset3BitDFI746DUI002Definition
  extends BitsCharsetDefinition(BitsCharset3BitDFI746DUI002)

object BitsCharset3BitDFI747DUI001 extends BitsCharsetNonByteSize {
  override lazy val name = "X-DFDL-3-BIT-DFI-747-DUI-001"
  override lazy val bitWidthOfACodeUnit = 3
  override lazy val decodeString = """AEGHJKLM"""
  override lazy val replacementCharCode = 0x0
  override lazy val requiredBitOrder = BitOrder.LeastSignificantBitFirst
}

final class BitsCharset3BitDFI747DUI001Definition
  extends BitsCharsetDefinition(BitsCharset3BitDFI747DUI001)

object BitsCharset5BitDFI769DUI002 extends BitsCharsetNonByteSize {
  override lazy val name = "X-DFDL-5-BIT-DFI-769-DUI-002"
  override lazy val bitWidthOfACodeUnit = 5
  override lazy val decodeString = """01234567ABCDEFGHJKLMNPQRSTUVWXYZ"""
  override lazy val replacementCharCode = 0x0
  override lazy val requiredBitOrder = BitOrder.LeastSignificantBitFirst
}

final class BitsCharset5BitDFI769DUI002Definition
  extends BitsCharsetDefinition(BitsCharset5BitDFI769DUI002)

object BitsCharset5BitDFI1661DUI001 extends BitsCharsetNonByteSize {
  override lazy val name = "X-DFDL-5-BIT-DFI-1661-DUI-001"
  override lazy val bitWidthOfACodeUnit = 5
  override lazy val decodeString =
    "\u00A0ABCDEFGHIJKLMNOPQRSTUVWXYZ\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD"
  override lazy val replacementCharCode = 0x0
  override lazy val requiredBitOrder = BitOrder.LeastSignificantBitFirst
}

final class BitsCharset5BitDFI1661DUI001Definition
  extends BitsCharsetDefinition(BitsCharset5BitDFI1661DUI001)
