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
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG._
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.LV
import scala.util.matching.Regex
import edu.illinois.ncsa.daffodil.dsom.Facet._
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType

// A Particle is something that can be repeating.
trait ParticleMixin { self: ElementBase =>

  override lazy val isScalar = minOccurs == 1 && maxOccurs == 1
  lazy val isRecurring = !isScalar

  override lazy val optMinOccurs: Option[Int] = Some(minOccurs)
  override lazy val optMaxOccurs: Option[Int] = Some(maxOccurs)

  /**
   * Determines if the element is optional, as in has zero or one instance only.
   *
   * There are two senses of optional
   * 1) Optional as in "might not be present" but for any reason.
   * Consistent with this is Required meaning must occur (but for any
   * reason. So all the occurrences of an array that has fixed number of
   * occurrences are required, and some of the occurrances of an array
   * that has a variable number of occurrences are optional.
   *
   * 2) Optional is in minOccurs="0" maxOccurs="1".
   *
   * Consistent with (2) is defining array as maxOccurs >= 2, and Required
   * as minOccurs=maxOccurs=1, but there are also special cases for occursCountKind parsed and stopValue
   * since they don't examine min/max occurs - they are only used for validation
   * in those occursCountKinds.
   *
   * The DFDL spec is not entirely consistent here either I don't believe.
   */
  lazy val isOptional = {
    // minOccurs == 0
    (optMinOccurs, optMaxOccurs) match {
      case (Some(1), Some(1)) => false // scalars are not optional
      case (Some(0), max) => {
        // now we must check on occursCountKind.
        // if parsed or stopValue then we consider it an array
        if (occursCountKind == OccursCountKind.Parsed ||
          occursCountKind == OccursCountKind.StopValue) {
          // we disregard the min/max occurs
          false
        } else {
          max match {
            case Some(1) => true
            case None => true
            case Some(_) => false
          }
        }
      }
      case _ => false
    }
  }

  override lazy val isArray = {
    // maxOccurs > 1 || maxOccurs == -1
    /**
     * Determines if the element is an array, as in can have more than one
     * instance.
     */
    if (isOptional) false
    else {
      val UNBOUNDED = -1
      (optMinOccurs, optMaxOccurs) match {
        case (None, None) => false
        case (Some(1), Some(1)) => false
        case (_, Some(n)) if n > 1 => true
        case (_, Some(UNBOUNDED)) => true
        /**
         * This next case is for occursCountKinds parsed and stopValue.
         * These only use min/maxOccurs for validation, so anything
         * with these occursCountKinds is an array (so long as it isn't
         * scalar)
         */
        case (_, Some(1)) if (occursCountKind == OccursCountKind.Parsed ||
          occursCountKind == OccursCountKind.StopValue) => true
        case _ => false
      }
    }
  }

  lazy val minOccurs = {
    val min = (self.xml \ "@minOccurs").text.toString
    min match {
      case "" => 1
      case _ => min.toInt
    }
  }

  lazy val maxOccurs = {
    val max = (self.xml \ "@maxOccurs").text.toString
    max match {
      case "unbounded" => -1
      case "" => 1
      case _ => max.toInt
    }
  }

  lazy val isFixedOccurrences = {
    // TODO optimizations to take scope into consideration. E.g.,
    // We could be in a context where the value of our occursCount expression
    // will always be a constant. 
    occursCountKind == OccursCountKind.Fixed
  }

  /**
   * Does this node have statically required instances.
   */
  lazy val hasStaticallyRequiredInstances = hasStaticallyRequiredInstances_.value
  private val hasStaticallyRequiredInstances_ = LV('hasStaticallyRequiredInstances) {
    val res =
      if (!isRepresented) false // if there's no rep, then it's not statically required.
      else if (isScalar) true
      else if (isFixedOccurrences) true
      else if (minOccurs > 0) true
      else false
    res
  }

  override lazy val isKnownRequiredElement = isKnownRequiredElement_.value
  private val isKnownRequiredElement_ = LV('isKnownRequiredElement) {
    if (isScalar) true
    else if (isFixedOccurrences) true
    else false
  }

  lazy val hasStopValue = hasStopValue_.value
  private val hasStopValue_ = LV('hasStopValue) {
    val sv = isRecurring && occursCountKind == OccursCountKind.StopValue
    // Don't check things like this aggressively. If we need occursStopValue then someone will ask for it.
    schemaDefinitionUnless(!(sv && occursStopValue == ""), "Property occursCountKind='stopValue' requires a non-empty occursStopValue property.")
    schemaDefinitionUnless(!sv, "occursCountKind='stopValue' is not implemented.")
    sv
  }
}

