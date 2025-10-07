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

import java.math.BigDecimal as JBigDecimal
import java.math.MathContext

import org.apache.daffodil.runtime1.infoset.DataValue.DataValueBigDecimal
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueBigInt
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueDouble
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueFloat
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueLong
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive

case object PlusDecimal extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBigDecimal = {
    v1.getBigDecimal.add(v2.getBigDecimal)
  }
}
case object MinusDecimal extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBigDecimal = {
    v1.getBigDecimal.subtract(v2.getBigDecimal)
  }
}
case object TimesDecimal extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBigDecimal = {
    v1.getBigDecimal.multiply(v2.getBigDecimal)
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
 *
 * Due to type promotion, it's actually fairly easy to write a simple expression
 * that leads to this error. For example, the expression "xs:int(1) div xs:double(.75)"
 * promotes both args to xs:decimal, and that division leads to non-terminating
 * decimal expansions. It is likely too difficult to require users to round these
 * basic expressions, so our implementation must handle this.
 *
 * DFDL expressions are supposed to be consistent with XPath, so we look there
 * for what our implementation should do. The XPath spec
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
 * So as long as we support 18 digits, our implementation should be in
 * conformance with the spec. To allow for high precision, our implementation
 * chooses to use MathContext.DECIMAL128, which provides 34 digits of precision
 * and a rounding mode of half-even.
 */
case object DivDecimal extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBigDecimal = {
    val v2bd = v2.getBigDecimal
    if (v2bd.compareTo(JBigDecimal.ZERO) == 0) {
      throw new ArithmeticException("/ by zero")
    }
    v1.getBigDecimal.divide(v2bd, MathContext.DECIMAL128)
  }
}
case object IDivDecimal extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBigInt = {
    DivDecimal.operate(v1, v2).getBigDecimal.toBigInteger
  }
}
case object ModDecimal extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBigDecimal = {
    v1.getBigDecimal.remainder(v2.getBigDecimal)
  }
}

case object PlusInteger extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBigInt = {
    v1.getBigInt.add(v2.getBigInt)
  }
}
case object MinusInteger extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBigInt = {
    v1.getBigInt.subtract(v2.getBigInt)
  }
}
case object TimesInteger extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBigInt = {
    v1.getBigInt.multiply(v2.getBigInt)
  }
}
case object DivInteger extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBigDecimal = {
    DivDecimal.operate(new JBigDecimal(v1.getBigInt), new JBigDecimal(v2.getBigInt))
  }
}
case object IDivInteger extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBigInt = {
    v1.getBigInt.divide(v2.getBigInt)
  }
}
case object ModInteger extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBigInt = {
    v1.getBigInt.mod(v2.getBigInt)
  }
}

case object PlusLong extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueLong = {
    v1.getLong + v2.getLong
  }
}
case object MinusLong extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueLong = {
    v1.getLong - v2.getLong
  }
}
case object TimesLong extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueLong = {
    v1.getLong * v2.getLong
  }
}
case object DivLong extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueDouble = {
    val v2l = v2.getLong
    if (v2l == 0) {
      throw new ArithmeticException("/ by zero")
    }
    v1.getLong.toDouble / v2l
  }
}
case object IDivLong extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueLong = {
    v1.getLong / v2.getLong
  }
}
case object ModLong extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueLong = {
    v1.getLong % v2.getLong
  }
}

case object PlusFloat extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueFloat = {
    v1.getFloat + v2.getFloat
  }
}
case object MinusFloat extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueFloat = {
    v1.getFloat - v2.getFloat
  }
}
case object TimesFloat extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueFloat = {
    v1.getFloat * v2.getFloat
  }
}
case object DivFloat extends NumericOp {
  // div float allows divide by zero to make infinity
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueFloat = {
    v1.getFloat / v2.getFloat
  }
}
case object IDivFloat extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueLong = {
    val v1f = v1.getFloat
    val v2f = v2.getFloat
    if (v2f == 0) {
      throw new ArithmeticException("/ by zero")
    }
    if (v1f.isInfinite || v1f.isNaN || v2f.isNaN) {
      throw new ArithmeticException("integer division with NaN or Infinity")
    }
    DivFloat.operate(v1, v2).getFloat.toLong
  }
}
case object ModFloat extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueFloat = {
    v1.getFloat % v2.getFloat
  }
}

case object PlusDouble extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueDouble = {
    v1.getDouble + v2.getDouble
  }
}
case object MinusDouble extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueDouble = {
    v1.getDouble - v2.getDouble
  }
}
case object TimesDouble extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueDouble = {
    v1.getDouble * v2.getDouble
  }
}
case object DivDouble extends NumericOp {
  // div double allows divide by zero to make infinity
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueDouble = {
    v1.getDouble / v2.getDouble
  }
}
case object IDivDouble extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueLong = {
    val v1d = v1.getDouble
    val v2d = v2.getDouble
    if (v2d == 0) {
      throw new ArithmeticException("/ by zero")
    }
    if (v1d.isInfinite || v1d.isNaN || v2d.isNaN) {
      throw new ArithmeticException("integer division with NaN or Infinity")
    }
    DivDouble.operate(v1, v2).getDouble.toLong
  }
}
case object ModDouble extends NumericOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueDouble = {
    v1.getDouble % v2.getDouble
  }
}
