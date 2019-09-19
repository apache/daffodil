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
package org.sgoodudfs.example.SupportedTypesFunctions

import org.apache.daffodil.udf.UserDefinedFunction
import org.apache.daffodil.udf.UserDefinedFunctionProvider
import org.apache.daffodil.udf.UserDefinedFunctionIdentification

/**
 * UDF Provider for Types Unit testing
 *
 */
class SupportedTypesFunctionsProvider extends UserDefinedFunctionProvider {
  override def getUserDefinedFunctionClasses =
    {
      Array(
        classOf[PrimByteFunc], classOf[BoxedByteFunc],
        classOf[PrimByteArrayFunc],
        classOf[PrimShortFunc], classOf[BoxedShortFunc],
        classOf[PrimLongFunc], classOf[BoxedLongFunc],
        classOf[PrimDoubleFunc], classOf[BoxedDoubleFunc],
        classOf[PrimFloatFunc], classOf[BoxedFloatFunc],
        classOf[PrimBooleanFunc], classOf[BoxedBooleanFunc],
        classOf[JavaBigDecimalFunc], classOf[JavaBigIntegerFunc])
    }
}

/**
 * UDF for testing primitive byte type as param/return
 *
 */
@UserDefinedFunctionIdentification(
  name = "primByteFunc",
  namespaceURI = "http://example.com/scala/audfs")
class PrimByteFunc extends UserDefinedFunction {
  def evaluate(arg: Byte) = {
    arg
  }
}

/**
 * UDF for testing primitive byte array type as param/return
 *
 */
@UserDefinedFunctionIdentification(
  name = "primByteArrayFunc",
  namespaceURI = "http://example.com/scala/audfs")
class PrimByteArrayFunc extends UserDefinedFunction {
  def evaluate(args: Array[Byte]) = {
    args
  }
}

/**
 * UDF for testing boxed byte type as param/return
 *
 */
@UserDefinedFunctionIdentification(
  name = "boxedByteFunc",
  namespaceURI = "http://example.com/scala/audfs")
class BoxedByteFunc extends UserDefinedFunction {
  def evaluate(arg: java.lang.Byte) = {
    arg
  }
}

/**
 * UDF for testing primitive short type as param/return
 *
 */
@UserDefinedFunctionIdentification(
  name = "primShortFunc",
  namespaceURI = "http://example.com/scala/audfs")
class PrimShortFunc extends UserDefinedFunction {
  def evaluate(arg: Short) = {
    arg
  }
}

/**
 * UDF for testing boxed short type as param/return
 *
 */
@UserDefinedFunctionIdentification(
  name = "boxedShortFunc",
  namespaceURI = "http://example.com/scala/audfs")
class BoxedShortFunc extends UserDefinedFunction {
  def evaluate(arg: java.lang.Short) = {
    arg
  }
}

/**
 * UDF for testing primitive long type as param/return
 *
 */
@UserDefinedFunctionIdentification(
  name = "primLongFunc",
  namespaceURI = "http://example.com/scala/audfs")
class PrimLongFunc extends UserDefinedFunction {
  def evaluate(arg: Long) = {
    arg
  }
}

/**
 * UDF for testing boxed long type as param/return
 *
 */
@UserDefinedFunctionIdentification(
  name = "boxedLongFunc",
  namespaceURI = "http://example.com/scala/audfs")
class BoxedLongFunc extends UserDefinedFunction {
  def evaluate(arg: java.lang.Long) = {
    arg
  }
}

/**
 * UDF for testing primitive double type as param/return
 *
 */
@UserDefinedFunctionIdentification(
  name = "primDoubleFunc",
  namespaceURI = "http://example.com/scala/audfs")
class PrimDoubleFunc extends UserDefinedFunction {
  def evaluate(arg: Double) = {
    arg
  }
}

/**
 * UDF for testing boxed double type as param/return
 *
 */
@UserDefinedFunctionIdentification(
  name = "boxedDoubleFunc",
  namespaceURI = "http://example.com/scala/audfs")
class BoxedDoubleFunc extends UserDefinedFunction {
  def evaluate(arg: java.lang.Double) = {
    arg
  }
}

/**
 * UDF for testing primitive float type as param/return
 *
 */
@UserDefinedFunctionIdentification(
  name = "primFloatFunc",
  namespaceURI = "http://example.com/scala/audfs")
class PrimFloatFunc extends UserDefinedFunction {
  def evaluate(arg: Float) = {
    arg
  }
}

/**
 * UDF for testing boxed float type as param/return
 *
 */
@UserDefinedFunctionIdentification(
  name = "boxedFloatFunc",
  namespaceURI = "http://example.com/scala/audfs")
class BoxedFloatFunc extends UserDefinedFunction {
  def evaluate(arg: java.lang.Float) = {
    arg
  }
}

/**
 * UDF for testing primitive boolean type as param/return
 *
 */
@UserDefinedFunctionIdentification(
  name = "primBooleanFunc",
  namespaceURI = "http://example.com/scala/audfs")
class PrimBooleanFunc extends UserDefinedFunction {
  def evaluate(arg: Boolean) = {
    arg
  }
}

/**
 * UDF for testing boxed boolean type as param/return
 *
 */
@UserDefinedFunctionIdentification(
  name = "boxedBooleanFunc",
  namespaceURI = "http://example.com/scala/audfs")
class BoxedBooleanFunc extends UserDefinedFunction {
  def evaluate(arg: java.lang.Boolean) = {
    arg
  }
}

/**
 * UDF for testing java big integer type as param/return
 *
 */
@UserDefinedFunctionIdentification(
  name = "javaBigIntegerFunc",
  namespaceURI = "http://example.com/scala/audfs")
class JavaBigIntegerFunc extends UserDefinedFunction {
  def evaluate(arg: java.math.BigInteger) = {
    arg
  }
}

/**
 * UDF for testing java big decimal type as param/return
 *
 */
@UserDefinedFunctionIdentification(
  name = "javaBigDecimalFunc",
  namespaceURI = "http://example.com/scala/audfs")
class JavaBigDecimalFunc extends UserDefinedFunction {
  def evaluate(arg: java.math.BigDecimal) = {
    arg
  }
}
