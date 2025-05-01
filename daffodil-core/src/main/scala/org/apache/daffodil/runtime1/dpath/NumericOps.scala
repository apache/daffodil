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

package org.apache.daffodil.runtime1.dpath

import java.lang.{ Number => JNumber }
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInt }

import org.apache.daffodil.lib.util.Numbers._

case object PlusDecimal extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigDecimal = {
    asBigDecimal(v1).add(asBigDecimal(v2))
  }
}
case object MinusDecimal extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigDecimal = {
    asBigDecimal(v1).subtract(asBigDecimal(v2))
  }
}
case object TimesDecimal extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigDecimal = {
    asBigDecimal(v1).multiply(asBigDecimal(v2))
  }
}

/**
 * Division with rounding for decimals
 *
 *  About the rounding.
 *
 *  Without rounding specified here, you can get an java arithmetic exception
 *  Specifically:
 *  {{{
 *  java.lang.ArithmeticException: Non-terminating decimal expansion; no exact representable decimal result.
 *  }}}
 *
 * What's questionable here, is to what number of fraction digits it will round.
 * That can be specified also as an argument to divide(), but we have no
 * information here (or anywhere really) about what precision is desired.
 * So we're omitting that and just saying "round it".
 *
 * Really, there's no rounding scale/precision until we're ready to represent
 * the number in a string. In this case we're not. We're in the middle of an
 * expression, just happen to have two BigDecimal operands, and we're dividing
 * them, which should produce a BigDecimal result.
 *
 * DFDL expressions are supposed to be consistent with XPath, so we look there
 * for suggestions. The XPath spec
 * [[https://www.w3.org/TR/xpath-functions-3/#op.numeric]] says it is implementation
 * defined.
 *
 *     For xs:decimal values, let N be the number of digits of precision
 *     supported by the implementation, and let M (M <= N) be the minimum
 *     limit on the number of digits required for conformance (18 digits
 *     for XSD 1.0, 16 digits for XSD 1.1). Then for addition, subtraction,
 *     and multiplication operations, the returned result should be accurate
 *     to N digits of precision, and for division and modulus operations,
 *     the returned result should be accurate to at least M digits of precision.
 *     The actual precision is ·implementation-defined·. If the number of digits
 *     in the mathematical result exceeds the number of digits that the
 *     implementation retains for that operation, the result is truncated
 *     or rounded in an ·implementation-defined· manner
 *
 *  In our case, the implementation does what the JVM and java libraries do when
 *  divide() is called with two arguments the second of which specifies
 *  to round half-up.
 */
case object DivDecimal extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigDecimal = {
    val v2bd = asBigDecimal(v2)
    if (v2bd.compareTo(JBigDecimal.ZERO) == 0) {
      throw new ArithmeticException("/ by zero")
    }
    asBigDecimal(v1).divide(v2bd)
  }
}
case object IDivDecimal extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(DivDecimal.operate(v1, v2)) }
}
case object ModDecimal extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigDecimal = {
    asBigDecimal(v1).remainder(asBigDecimal(v2))
  }
}

case object PlusInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).add(asBigInt(v2)) }
}
case object MinusInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).subtract(asBigInt(v2)) }
}
case object TimesInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).multiply(asBigInt(v2)) }
}
case object DivInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigDecimal = { DivDecimal.operate(v1, v2) }
}
case object IDivInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(DivInteger.operate(v1, v2)) }
}
case object ModInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).mod(asBigInt(v2)) }
}

case object PlusNonNegativeInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).add(asBigInt(v2)) }
}
case object MinusNonNegativeInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).subtract(asBigInt(v2)) }
}
case object TimesNonNegativeInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).multiply(asBigInt(v2)) }
}
case object DivNonNegativeInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigDecimal = { DivDecimal.operate(v1, v2) }
}
case object IDivNonNegativeInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = {
    asBigInt(DivNonNegativeInteger.operate(v1, v2))
  }
}
case object ModNonNegativeInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).mod(asBigInt(v2)) }
}

case object PlusUnsignedLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).add(asBigInt(v2)) }
}
case object MinusUnsignedLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).subtract(asBigInt(v2)) }
}
case object TimesUnsignedLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).multiply(asBigInt(v2)) }
}
case object DivUnsignedLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigDecimal = { DivDecimal.operate(v1, v2) }
}
case object IDivUnsignedLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(DivUnsignedLong.operate(v1, v2)) }
}
case object ModUnsignedLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).mod(asBigInt(v2)) }
}

case object PlusLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(v1) + asLong(v2) }
}
case object MinusLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(v1) - asLong(v2) }
}
case object TimesLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(v1) * asLong(v2) }
}
case object DivLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigDecimal = { DivDecimal.operate(v1, v2) }
}
case object IDivLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(DivLong.operate(v1, v2)) }
}
case object ModLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(v1) % asLong(v2) }
}

case object PlusUnsignedInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(v1) + asLong(v2) }
}
case object MinusUnsignedInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(v1) - asLong(v2) }
}
case object TimesUnsignedInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(v1) * asLong(v2) }
}
case object DivUnsignedInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { DivDecimal.operate(v1, v2) }
}
case object IDivUnsignedInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(DivUnsignedInt.operate(v1, v2)) }
}
case object ModUnsignedInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(v1) % asLong(v2) }
}

case object PlusInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(v1) + asInt(v2) }
}
case object MinusInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(v1) - asInt(v2) }
}
case object TimesInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(v1) * asInt(v2) }
}
case object DivInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { DivDecimal.operate(v1, v2) }
}
case object IDivInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(DivInt.operate(v1, v2)) }
}
case object ModInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(v1) % asInt(v2) }
}

case object PlusUnsignedShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(v1) + asInt(v2) }
}
case object MinusUnsignedShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(v1) - asInt(v2) }
}
case object TimesUnsignedShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(v1) * asInt(v2) }
}
case object DivUnsignedShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { DivDecimal.operate(v1, v2) }
}
case object IDivUnsignedShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(DivUnsignedShort.operate(v1, v2)) }
}
case object ModUnsignedShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(v1) % asInt(v2) }
}

case object PlusShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(v1) + asShort(v2) }
}
case object MinusShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(v1) - asShort(v2) }
}
case object TimesShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(v1) * asShort(v2) }
}
case object DivShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { DivDecimal.operate(v1, v2) }
}
case object IDivShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(DivShort.operate(v1, v2)) }
}
case object ModShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(v1) % asShort(v2) }
}

case object PlusUnsignedByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(v1) + asShort(v2) }
}
case object MinusUnsignedByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(v1) - asShort(v2) }
}
case object TimesUnsignedByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(v1) * asShort(v2) }
}
case object DivUnsignedByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { DivDecimal.operate(v1, v2) }
}
case object IDivUnsignedByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(DivUnsignedByte.operate(v1, v2)) }
}
case object ModUnsignedByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(v1) % asShort(v2) }
}

case object PlusByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asByte(v1) + asByte(v2) }
}
case object MinusByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asByte(v1) - asByte(v2) }
}
case object TimesByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asByte(v1) * asByte(v2) }
}
case object DivByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { DivDecimal.operate(v1, v2) }
}
case object IDivByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asByte(DivByte.operate(v1, v2)) }
}
case object ModByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asByte(v1) % asByte(v2) }
}

case object PlusFloat extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asFloat(v1) + asFloat(v2) }
}
case object MinusFloat extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asFloat(v1) - asFloat(v2) }
}
case object TimesFloat extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asFloat(v1) * asFloat(v2) }
}
case object DivFloat extends NumericOp {
  // div float allows divide by zero to make infinity
  def operate(v1: JNumber, v2: JNumber): JNumber = { asFloat(v1) / asFloat(v2) }
}
case object IDivFloat extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = {
    val v1f = asFloat(v1)
    val v2f = asFloat(v2)
    if (v2f == 0) {
      throw new ArithmeticException("/ by zero")
    }
    if (v1f.isInfinite || v1f.isNaN || v2f.isNaN) {
      throw new ArithmeticException("integer division with NaN or Infinity")
    }
    asInt(DivFloat.operate(v1, v2))
  }
}
case object ModFloat extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asFloat(v1) % asFloat(v2) }
}

case object PlusDouble extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asDouble(v1) + asDouble(v2) }
}
case object MinusDouble extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asDouble(v1) - asDouble(v2) }
}
case object TimesDouble extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asDouble(v1) * asDouble(v2) }
}
case object DivDouble extends NumericOp {
  // div double allows divide by zero to make infinity
  def operate(v1: JNumber, v2: JNumber): JNumber = { asDouble(v1) / asDouble(v2) }
}
case object IDivDouble extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = {
    val v1d = asDouble(v1)
    val v2d = asDouble(v2)
    if (v2d == 0) {
      throw new ArithmeticException("/ by zero")
    }
    if (v1d.isInfinite || v1d.isNaN || v2d.isNaN) {
      throw new ArithmeticException("integer division with NaN or Infinity")
    }
    asLong(DivDouble.operate(v1, v2))
  }
}
case object ModDouble extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asDouble(v1) % asDouble(v2) }
}
