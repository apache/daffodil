/* Copyright (c) 2017 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.util

import edu.illinois.ncsa.daffodil.exceptions.Assert

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
