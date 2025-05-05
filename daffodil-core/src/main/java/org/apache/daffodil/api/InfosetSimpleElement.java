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

package org.apache.daffodil.api;


import com.ibm.icu.util.Calendar;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URI;

/**
 * Methods specific to simple elements in the infoset
 */
public interface InfosetSimpleElement extends InfosetElement {
  /*
   * Access to the metadata information about this element.
   * See {@link SimpleElementMetadata}
   */
  @Override
  SimpleElementMetadata metadata();

  /**
   * Obtains the value, then converts it to a string.
   * Caches the string so we're not allocating strings repeatedly
   */
  String getText();

  /*
   * These are so that API users don't have to know about our
   * very Scala-oriented DataValue type system.
   */

  /**
   * @return the value of this simple element as a Scala AnyRef, which is
   * equivalent to a Java Object.
   */
  Object getAnyRef();

  /**
   * @return the value of this simple element as an Object (java.lang.Object),
   * which is equivalent to Scala AnyRef.
   */
  default Object getObject() {
    return getAnyRef();
  }

  /**
   * @return Casts the value of this simple element as a java.math.BigDecimal
   * or {@throws InfosetTypeException} if the element is not of type decimal.
   */
  BigDecimal getDecimal();

  /**
   * @return the value of this simple element cast as a `com.ibm.icu.util.Calendar`.
   * or {@throws InfosetTypeException} if the element is not of type date.
   */
  Calendar getDate();

  /**
   * @return the value of this simple element as a `com.ibm.icu.util.Calendar`.
   * or {@throws InfosetTypeException} if the element is not of type time.
   */
  Calendar getTime();

  /**
   * @return the value of this simple element cast to `com.ibm.icu.util.Calendar`.
   * or {@throws InfosetTypeException} if the element is not of type dateTime.
   */
  Calendar getDateTime();

  /**
   * @return the value of this simple element of HexBinary type cast to `Array[Byte]`.
   * or {@throws InfosetTypeException} if the element is not of type hexBinary.
   */
  byte[] getHexBinary();

  /**
   * @return the value of this simple element of Boolean type cast to java.lang.Boolean.
   * or {@throws InfosetTypeException} if the element is not of type boolean.
   */
  Boolean getBoolean();

  /**
   * Used to access simple element values of all integer types representable by
   * a 64 bit signed java.lang.Long.
   * <p>
   * The separate {@code getUnsignedLong}
   * must be used for the DFDL unsignedLong type.
   *
   * @return the value of this simple element converted to java.lang.Long.
   * or {@throws InfosetTypeException} if the element value is not convertible to the result type.
   */
  Long getLong();

  /**
   * Used to access simple element values of all integer types representable by
   * a 32 bit signed java.lang.Integer.
   * <p>
   * The separate {@code getUnsignedInt}
   * must be used for the DFDL unsignedInt type.
   * <p>
   * Do not confuse DFDL integer type with java.lang.Integer, which is the object version of
   * a java.lang.int, which is limited to only signed 32-bits of magnitude. The DFDL integer
   * type is an unbounded magnitude integer (aka BigInteger).
   *
   * @return the value of this simple element converted to java.lang.Integer.
   * or {@throws InfosetTypeException} if the element value is not convertible to the result type.
   */
  Integer getInt();

  /**
   * Used to access simple element values of all integer types representable by
   * a 16 bit signed java.lang.Short.
   * <p>
   * The separate {@code  getUnsignedShort}
   * must be used for the DFDL unsignedShorttype.
   *
   * @return the value of this simple element converted to java.lang.Short.
   * or {@throws InfosetTypeException} if the element value is not convertible to the result type.
   */
  Short getShort();

  /**
   * Used to access simple element values of all integer types representable by
   * an 8-bit signed java.lang.Byte.
   * <p>
   * The separate {@code getUnsignedByte}
   * must be used for the DFDL unsignedBytetype.
   *
   * @return the value of this simple element converted to java.lang.Short.
   * or {@throws InfosetTypeException} if the element value is not convertible to the result type.
   */
  Byte getByte();

  /**
   * Used to access simple element values of all non-negative integer types representable by
   * a 32-bit unsigned integer.
   * <p>
   * Note that the returned value is the larger signed type, java.lang.Long which is capable
   * of representing unsigned integer values greater than java.lang.Integer.MAX_VALUE.
   *
   * @return the value of this simple element converted to java.lang.Long.
   * or {@throws InfosetTypeException} if the element value is not convertible to the result type.
   */
  Long getUnsignedInt();

  /**
   * Used to access simple element values of all non-negative integer types representable by
   * a 16-bit unsigned integer.
   * <p>
   * Note that the returned value is the larger signed type, java.lang.Int which is capable
   * of representing unsigned integer values greater than java.lang.Short.MAX_VALUE.
   *
   * @return the value of this simple element converted to java.lang.Int.
   * or {@throws InfosetTypeException} if the element value is not convertible to the result type.
   */
  Integer getUnsignedShort();

  /**
   * Used to access simple element values of all non-negative integer types representable by
   * an 8-bit unsigned integer.
   * <p>
   * Note that the returned value is the larger signed type, java.lang.Short which is capable
   * of representing unsigned integer values greater than java.lang.Byte.MAX_VALUE.
   *
   * @return the value of this simple element converted to java.lang.Int.
   * or {@throws InfosetTypeException} if the element value is not convertible to the result type.
   */
  Short getUnsignedByte();

  /**
   * Used to access simple element values of all non-negative integer types representable by
   * a 64-bit unsigned integer.
   * <p>
   * Note that the returned value is the larger signed type, java.math.BigInteger which is capable
   * of representing unsigned integer values greater than java.lang.Long.MAX_VALUE.
   *
   * @return the value of this simple element converted to java.lang.BigInteger.
   * or {@throws InfosetTypeException} if the element value is not convertible to the result type.
   */
  BigInteger getUnsignedLong();

  /**
   * @return the value of this simple element of Double type cast to java.lang.Double.
   * or {@throws InfosetTypeException} if the element in not of type double.
   */
  Double getDouble();

  /**
   * @return the value of this simple element of Float type cast to java.lang.Float.
   * or {@throws InfosetTypeException} if the element is not of type float.
   */
  Float getFloat();

  /**
   * Used to get the value of DFDL `integer` type, which is an unbounded-magnitude integer.
   * <p>
   * Do not confuse DFDL integer type with java.lang.Integer, which is the object version of
   * a java.lang.int, which is limited to only signed 32-bits of magnitude.
   *
   * @return the value of this simple element of Integer type cast to java.math.BigInteger.
   * or {@throws InfosetTypeException} if the element is not of type integer.
   */
  BigInteger getInteger();

  /**
   * @return the value of this simple element of NonNegativeInteger type cast to java.math.BigInteger.
   * or {@throws InfosetTypeException} if the element value is not convertible to the result type.
   */
  BigInteger getNonNegativeInteger();

  /**
   * @return the value of this simple element of String type cast to java.lang.String.
   * or {@throws InfosetTypeException} if the element value is not of String type.
   */
  String getString();

  /**
   * @return the value of this simple element of URI type cast to java.net.URI.
   * or {@throws InfosetTypeException} if the element value is not of URI type.
   */
  URI getURI();
}
