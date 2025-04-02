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

package org.apache.daffodil.lib.util

import org.apache.daffodil.lib.exceptions.Assert

/**
 * Indentable provides a simple interface to keep track of indentation levels
 * and output an indent string efficiently. This makes efforts to reduce String
 * allocations by maintaining a single string of whitespace characters and
 * outputting only the appropriate number of characters when indenting. This can
 * be mixed into any object to support indenting and can output to any
 * java.io.Writer.
 */
trait Indentable {

  /**
   * The number of spaces representing a single level of indentation. Can be
   * overridden to change the number of spaces.
   */
  val indentSize: Int = 2

  /**
   * The character to use for indentation. This can be overridden to indent
   * with a different character (e.g. \t for tabs)
   */
  val indentChar: Char = ' '

  /**
   * A preallocated string of spaces reused for indentation, initialized to 10
   * indentation levels. This will grow automatically in size if the
   * indentation level grows beyond 10 levels.
   */
  private var indentString: String = indentChar.toString * (indentSize * 10)

  /**
   * The number of characters from indentString to write when we needed
   * indentation. This takes into account indentSize so no extra calculation is
   * needed when outputting indentation.
   */
  private var indentLength: Int = 0

  /**
   * Reset the indentation level to zero.
   */
  final def resetIndentation(): Unit = {
    indentLength = 0
  }

  /**
   * Increase the indentation level by one.
   */
  final def incrementIndentation(): Unit = {
    indentLength += indentSize
    if (indentString.length < indentLength) {
      // double the size of the indent string if our indentation level got
      // too big for our preallocated string of spaces
      indentString = indentString + indentString
    }
  }

  /**
   * Decrease the indentation level by one.
   */
  final def decrementIndentation(): Unit = {
    indentLength -= indentSize
    Assert.invariant(indentLength >= 0)
  }

  /**
   * Write the appropriate number of spaces to the Writable output.
   */
  final def outputIndentation(output: java.io.Writer): Unit = {
    output.write(indentString, 0, indentLength)
  }

  /**
   * Return the indentation level.
   */
  final def getIndentationLevel(): Int = {
    indentLength / indentSize
  }
}
