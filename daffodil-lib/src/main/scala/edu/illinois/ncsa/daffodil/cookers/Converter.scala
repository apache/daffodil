/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.cookers

import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE

/*
 * Quick tutorial on -Before +After.
 *
 * Use of -Before makes it contravariant.
 *
 * To convince myself, I'm going to walk through the contravariance "thing".....
 *
 * An example. Let's assume JByte <: JShort <: JInt. (reminder <: is the scala "is a subtype of" operator)
 *
 * Now, given a Converter[JShort, String], the convert method takes a JShort and creates a String.
 *
 * If I want to create a derived Converter2[X, String] <: Converter[JShort, String] what can type X be?
 *
 * For one converter to be a subtype of another, the derived one has to be able to substitute for the base anywhere that base occurs.
 *
 * This means that the convert method of the derived must accept at least a JShort (which is what the convert of the base class accepts), but
 * convert of the derived may be more general, and accept more than that.
 *
 * Hence Converter[JInt, String] <: Converter[JShort, String] even though JInt >: JShort
 *
 * That's what contravariance means. To create a subtype it doesn't narrow with the derivation, it broadens with the derivation.
 *
 * So yes I'm convinced Converter should be Converter[-Before, +After]
 */
trait Converter[-Before, +After] extends Serializable {

  /**
   * Override these runtime and constant-specific variants if you need different
   * conversions done if an expression is evaluated at runtime.
   *
   * This is for things like delimiters or escapeCharacter where it can be "" if
   * it is a constant, but it cannot be "" if it is a runtime expression value.
   *
   * The forUnparse flag is for distinctions needed between parse time and unparse time
   * which is very common thing to have.
   *
   * This is not the same concept as Compile time vs. Runtime.
   */
  def convertRuntime(b: Before, context: ThrowsSDE, forUnparse: Boolean): After =
    convert(b, context, forUnparse)

  def convertConstant(b: Before, context: ThrowsSDE, forUnparse: Boolean): After =
    convert(b, context, forUnparse)

  /**
   * Override this if there is just one conversion used for both constants and
   * for runtime expressions.
   */
  protected def convert(b: Before, context: ThrowsSDE, forUnparse: Boolean): After

}
