/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.dsom

import scala.xml.Node
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.api.WithDiagnostics
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import scala.util.matching.Regex
import edu.illinois.ncsa.daffodil.dsom.Facet._
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType

abstract class LocalElementBase(xmlArg: Node, parent: SchemaComponent, position: Int)
  extends ElementBase(xmlArg, parent, position)
  with LocalElementMixin {

  requiredEvaluations(checkForAlignmentAmbiguity)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private def lcm(a: Int, b: Int): Int = math.abs(a * b) / gcd(a, b)
  private def isXAMultipleOfY(x: Int, y: Int): Boolean = (x % y) == 0

  private def isAlignmentCompatible(current: Int, next: Int): Boolean = {
    isXAMultipleOfY(current, next)
  }

  /**
   * Changed to a warning - DFDL WG decided to make this check optional, but it
   * is still useful as a warning.
   *
   * Turns out that MIL STD 2045 header format needs to pad out to a byte boundary
   * at the end of the structure. An optional, non-byte aligned field precedes
   * the end of the structure; hence, putting a zero-length byte-aligned field
   * at the end was crashing into this error. I couldn't think of a work-around,
   * so changed this into a warning.
   *
   * The old requirement was:
   *   To avoid ambiguity when parsing, optional elements and variable-occurrence arrays
   *   where the minimum number of occurrences is zero cannot have alignment properties
   *   different from the items that follow them. It is a schema definition error otherwise.
   *
   * Part of the required evaluations for LocalElementBase.
   */
  final def checkForAlignmentAmbiguity: Unit = {
    if (isOptional) {
      this.couldBeNext.filterNot(m => m == thisTermNoRefs).foreach { that =>
        val isSame = this.alignmentValueInBits == that.alignmentValueInBits
        if (!isSame) {
          this.SDW("%s is an optional element or a variable-occurrence array and its alignment (%s) is not the same as %s's alignment (%s).",
            this.toString, this.alignmentValueInBits, that.toString, that.alignmentValueInBits)
        }
      }
    }
  }

}

