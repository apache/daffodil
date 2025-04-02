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

package org.apache.daffodil.runtime1.iapi

import java.lang.{ Boolean => JBoolean }
import java.lang.{ Byte => JByte }
import java.lang.{ Double => JDouble }
import java.lang.{ Float => JFloat }
import java.lang.{ Integer => JInt }
import java.lang.{ Long => JLong }
import java.lang.{ Short => JShort }
import java.lang.{ String => JString }
import java.math.{ BigDecimal => JBigDecimal }
import java.math.{ BigInteger => JBigInt }
import java.net.URI

import com.ibm.icu.util.Calendar

/**
 * API access to array objects in the DFDL Infoset
 */
trait InfosetArray extends InfosetItem {

  /**
   * @return the metadata of the element that is an array
   */
  override def metadata: ElementMetadata
}

/**
 * API access to elements of the DFDL Infoset of both
 * complex and simple type.
 */
trait InfosetElement extends InfosetItem {

  /**
   * In DFDL both simple and complex type elements can be
   * nilled.
   *
   * @return true if the element is nilled, false otherwise.
   */
  def isNilled: Boolean

  /*
   * Access to the metadata information about this element.
   * See [[ElementMetadata]]
   */
  def metadata: ElementMetadata

}

/**
 * Methods specific complex elements in the infoset
 */
trait InfosetComplexElement extends InfosetElement {

  /*
   * Access to the metadata information about this element.
   * See [[ComplexElementMetadata]]
   */
  override def metadata: ComplexElementMetadata
}

/**
 * Methods specific to simple elements in the infoset
 */
trait InfosetSimpleElement extends InfosetElement {

  /*
   * Access to the metadata information about this element.
   * See [[SimpleElementMetadata]]
   */
  override def metadata: SimpleElementMetadata

  /**
   * Obtains the value, then converts it to a string.
   * Caches the string so we're not allocating strings repeatedly
   */
  def getText: String

  /*
   * These are so that API users don't have to know about our
   * very Scala-oriented DataValue type system.
   */

  /**
   * @return the value of this simple element as a Scala AnyRef, which is
   *         equivalent to a Java Object.
   */
  def getAnyRef: AnyRef

  /**
   * @return the value of this simple element as an Object (java.lang.Object),
   *         which is equivalent to Scala AnyRef.
   */
  final def getObject: java.lang.Object = getAnyRef

  // Note: I could not get @throws in scaladoc to work right.
  // Complains "Could not find any member to link for ... and I tried various formulations of
  // InfosetTypeException, with package, with and without [[..]].
  // So I've just converted it to plain text.

  /**
   * @return Casts the value of this simple element as a java.math.BigDecimal
   * or throws `InfosetTypeException` if the element is not of type decimal.
   */
  def getDecimal: JBigDecimal

  /**
   * @return the value of this simple element cast as a `com.ibm.icu.util.Calendar`.
   * or throws `InfosetTypeException` if the element is not of type date.
   */
  def getDate: Calendar

  /**
   * @return the value of this simple element as a `com.ibm.icu.util.Calendar`.
   * or throws `InfosetTypeException` if the element is not of type time.
   */
  def getTime: Calendar

  /**
   * @return the value of this simple element cast to `com.ibm.icu.util.Calendar`.
   * or throws `InfosetTypeException` if the element is not of type dateTime.
   */
  def getDateTime: Calendar

  /**
   * @return the value of this simple element of HexBinary type cast to `Array[Byte]`.
   * or throws `InfosetTypeException` if the element is not of type hexBinary.
   */
  def getHexBinary: Array[Byte]

  /**
   * @return the value of this simple element of Boolean type cast to java.lang.Boolean.
   * or throws `InfosetTypeException` if the element is not of type boolean.
   */
  def getBoolean: JBoolean

  /**
   * Used to access simple element values of all integer types representable by
   * a 64 bit signed java.lang.Long.
   *
   * The separate [[getUnsignedLong]]
   * must be used for the DFDL unsignedLong type.
   * @return the value of this simple element converted to java.lang.Long.
   * or throws `InfosetTypeException` if the element value is not convertible to the result type.
   */
  def getLong: JLong

  /**
   * Used to access simple element values of all integer types representable by
   * a 32 bit signed java.lang.Integer.
   *
   * The separate [[getUnsignedInt]]
   * must be used for the DFDL unsignedInt type.
   *
   * Do not confuse DFDL integer type with java.lang.Integer, which is the object version of
   * a java.lang.int, which is limited to only signed 32-bits of magnitude. The DFDL integer
   * type is an unbounded magnitude integer (aka BigInteger).
   *
   * @return the value of this simple element converted to java.lang.Integer.
   * or throws `InfosetTypeException` if the element value is not convertible to the result type.
   */
  def getInt: JInt

  /**
   * Used to access simple element values of all integer types representable by
   * a 16 bit signed java.lang.Short.
   *
   * The separate [[getUnsignedShort]]
   * must be used for the DFDL unsignedShorttype.
   * @return the value of this simple element converted to java.lang.Short.
   * or throws `InfosetTypeException` if the element value is not convertible to the result type.
   */
  def getShort: JShort

  /**
   * Used to access simple element values of all integer types representable by
   * an 8-bit signed java.lang.Byte.
   *
   * The separate [[getUnsignedByte]]
   * must be used for the DFDL unsignedBytetype.
   *
   * @return the value of this simple element converted to java.lang.Short.
   * or throws `InfosetTypeException` if the element value is not convertible to the result type.
   */
  def getByte: JByte

  /**
   * Used to access simple element values of all non-negative integer types representable by
   * a 32-bit unsigned integer.
   *
   * Note that the returned value is the larger signed type, java.lang.Long which is capable
   * of representing unsigned integer values greater than java.lang.Integer.MAX_VALUE.
   *
   * @return the value of this simple element converted to java.lang.Long.
   * or throws `InfosetTypeException` if the element value is not convertible to the result type.
   */
  def getUnsignedInt: JLong

  /**
   * Used to access simple element values of all non-negative integer types representable by
   * a 16-bit unsigned integer.
   *
   * Note that the returned value is the larger signed type, java.lang.Int which is capable
   * of representing unsigned integer values greater than java.lang.Short.MAX_VALUE.
   *
   * @return the value of this simple element converted to java.lang.Int.
   * or throws `InfosetTypeException` if the element value is not convertible to the result type.
   */
  def getUnsignedShort: JInt

  /**
   * Used to access simple element values of all non-negative integer types representable by
   * an 8-bit unsigned integer.
   *
   * Note that the returned value is the larger signed type, java.lang.Short which is capable
   * of representing unsigned integer values greater than java.lang.Byte.MAX_VALUE.
   *
   * @return the value of this simple element converted to java.lang.Int.
   * or throws `InfosetTypeException` if the element value is not convertible to the result type.
   */
  def getUnsignedByte: JShort

  /**
   * Used to access simple element values of all non-negative integer types representable by
   * a 64-bit unsigned integer.
   *
   * Note that the returned value is the larger signed type, java.math.BigInteger which is capable
   * of representing unsigned integer values greater than java.lang.Long.MAX_VALUE.
   *
   * @return the value of this simple element converted to java.lang.BigInteger.
   * or throws `InfosetTypeException` if the element value is not convertible to the result type.
   */
  def getUnsignedLong: JBigInt

  /**
   * @return the value of this simple element of Double type cast to java.lang.Double.
   * or throws `InfosetTypeException` if the element in not of type double.
   */
  def getDouble: JDouble

  /**
   * @return the value of this simple element of Float type cast to java.lang.Float.
   * or throws `InfosetTypeException` if the element is not of type float.
   */
  def getFloat: JFloat

  /**
   * Used to get the value of DFDL `integer` type, which is an unbounded-magnitude integer.
   *
   * Do not confuse DFDL integer type with java.lang.Integer, which is the object version of
   * a java.lang.int, which is limited to only signed 32-bits of magnitude.
   * @return the value of this simple element of Integer type cast to java.math.BigInteger.
   * or throws `InfosetTypeException` if the element is not of type integer.
   */
  def getInteger: JBigInt

  /**
   * @return the value of this simple element of NonNegativeInteger type cast to java.math.BigInteger.
   * or throws `InfosetTypeException` if the element value is not convertible to the result type.
   */
  def getNonNegativeInteger: JBigInt

  /**
   * @return the value of this simple element of String type cast to java.lang.String.
   * or throws `InfosetTypeException` if the element value is not of String type.
   */
  def getString: JString

  /**
   * @return the value of this simple element of URI type cast to java.net.URI.
   * or throws `InfosetTypeException` if the element value is not of URI type.
   */
  def getURI: URI
}

// $COVERAGE-OFF$
/**
 * Thrown if you try to access a simple type but the value of the
 * InfosetSimpleElement is not convertible to that type.
  */
class InfosetTypeException(msg: String, cause: Throwable)
  extends Exception(msg: String, cause: Throwable) {

  def this(msg: String) = this(msg, null)
  def this(cause: Throwable) = this(null, cause)
}
// $COVERAGE-ON$

/**
 * Access to the infoset document element (also known as the root element).
 */
trait InfosetDocument extends InfosetItem {

  /**
   * Access to the metadata information about this element.
   * See [[ElementMetadata]]
   */
  override def metadata: ElementMetadata
}

/**
 * Methods common to all infoset items
 */
trait InfosetItem {

  /**
   * All infoset items have access to metadata.
   */
  def metadata: Metadata
}
