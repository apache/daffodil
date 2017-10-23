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

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.equality._

trait RequiredOptionalMixin { self: ElementBase =>

  def minOccurs: Int
  def maxOccurs: Int

  final override lazy val isScalar = minOccurs == 1 && maxOccurs == 1

  final override lazy val isOptional = {
    // minOccurs == 0
    (optMinOccurs, optMaxOccurs) match {
      case (Some(1), Some(1)) => false // scalars are not optional
      case (Some(0), max) => {
        // now we must check on occursCountKind.
        // if parsed or stopValue then we consider it an array
        if (occursCountKind =:= OccursCountKind.Parsed ||
          occursCountKind =:= OccursCountKind.StopValue ||
          occursCountKind =:= OccursCountKind.Expression) {
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

  final override def isRequired: Boolean = LV('isRequired) {
    val res = {
      if (isScalar) true
      else if (isOptional) false
      else if (isArray) isRequiredArrayElement
      else false
    }
    res
  }.value

  final override lazy val isArray = {
    // maxOccurs > 1 || maxOccurs == -1

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
          occursCountKind == OccursCountKind.StopValue ||
          occursCountKind == OccursCountKind.Expression) => true
        case _ => false
      }
    }
  }

  final lazy val isRequiredArrayElement = {
    isArray &&
      minOccurs > 0 &&
      (occursCountKind == OccursCountKind.Fixed ||
        occursCountKind == OccursCountKind.Implicit ||
        occursCountKind == OccursCountKind.Expression)
  }
}

// A Particle is something that can be repeating.
trait ParticleMixin extends RequiredOptionalMixin { self: ElementBase =>

  final lazy val optMinOccurs: Option[Int] = Some(minOccurs)
  final lazy val optMaxOccurs: Option[Int] = Some(maxOccurs)

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

  final lazy val isFixedOccurrences = {
    // TODO optimizations to take scope into consideration. E.g.,
    // We could be in a context where the value of our occursCount expression
    // will always be a constant.
    occursCountKind == OccursCountKind.Fixed
  }

  /**
   * Does this node have statically required instances.
   */
  final def hasStaticallyRequiredInstances = LV('hasStaticallyRequiredInstances) {
    val res =
      if (!isRepresented) false // if there's no rep, then it's not statically required.
      else if (isScalar) true
      else if (isFixedOccurrences) true
      else if (minOccurs > 0) true
      else false
    res
  }.value

  final override def isKnownRequiredElement = LV('isKnownRequiredElement) {
    if (isScalar) true
    else if (isFixedOccurrences) true
    else false
  }.value

  final lazy val hasStopValue = LV('hasStopValue) {
    val sv = !isScalar && occursCountKind == OccursCountKind.StopValue
    // Don't check things like this aggressively. If we need occursStopValue then someone will ask for it.
    schemaDefinitionUnless(!(sv && occursStopValue == ""), "Property occursCountKind='stopValue' requires a non-empty occursStopValue property.")
    schemaDefinitionUnless(!sv, "occursCountKind='stopValue' is not implemented.")
    sv
  }.value
}
