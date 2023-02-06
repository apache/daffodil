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

package org.apache.daffodil.unparsers.runtime1

import org.apache.daffodil.lib.util.MaybeChar

/**
 * When dfdl:lengthKind is 'explicit' (and dfdl:length is an expression),
 * 'delimited', 'prefixed', 'pattern' the data value is padded to the length given
 * by the XSD minLength facet for type 'xs:string' or
 * dfdl:textOutputMinLength property for other types.
 *
 *
 * dfdl:textOutputMinLength:
 * Only used when dfdl:textPadKind is 'padChar' and dfdl:lengthKind is
 * 'delimited', 'prefixed', 'pattern', 'explicit' (when dfdl:length is an expression)
 * or 'endOfParent', and type is not xs:string
 * Specifies the minimum content length during unparsing for simple types
 * that do not allow the XSD minLength facet to be specified.
 * For dfdl:lengthKind 'delimited', 'pattern' and 'endOfParent' the length units
 * are always characters, for other dfdl:lengthKinds the length units are
 * specified by the dfdl:lengthUnits property.
 * If dfdl:textOutputMinLength is zero or less than the length of the
 * representation text then no padding occurs.
 */
trait PaddingRuntimeMixin {

  def pad: MaybeChar
  def padToLength: Int

  def addRightPadding(str: String): String = {
    val inputLength = str.length

    val res =
      if (!pad.isDefined || padToLength < inputLength || padToLength == 0) { str }
      else {
        val numCharsToAdd = padToLength - inputLength
        addRightPadding(str, numCharsToAdd)
      }
    res
  }

  def addLeftPadding(str: String): String = {
    val inputLength = str.length

    val res =
      if (!pad.isDefined || padToLength < inputLength || padToLength == 0) { str }
      else {
        val numCharsToAdd = padToLength - inputLength
        addLeftPadding(str, numCharsToAdd)
      }
    res
  }

  def addPadding(str: String): String = {
    val inputLength = str.length

    val res = if (!pad.isDefined || padToLength < inputLength || padToLength == 0) { str }
    else {
      val numCharsToAddTotal = padToLength - inputLength
      val numAddToLeft = Math.ceil(numCharsToAddTotal / 2).toInt
      val numAddToRight = numCharsToAddTotal - numAddToLeft

      addRightPadding(addLeftPadding(str, numAddToLeft), numAddToRight)
    }

    res
  }

  private def append(sb: StringBuilder, numCharsToAppend: Int): StringBuilder = {
    for (i <- 1 to numCharsToAppend) { sb.append(pad.get) }
    sb
  }

  private def addRightPadding(str: String, numCharsToPad: Int): String = {
    val sb = append(new StringBuilder(str), numCharsToPad)
    sb.toString
  }
  private def addLeftPadding(str: String, numCharsToPad: Int): String = {
    val sb = append(new StringBuilder, numCharsToPad)
    sb.append(str)
    sb.toString
  }

}
