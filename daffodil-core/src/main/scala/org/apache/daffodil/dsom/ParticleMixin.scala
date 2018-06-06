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

package org.apache.daffodil.dsom

import org.apache.daffodil.schema.annotation.props.gen._
import org.apache.daffodil.equality._

trait RequiredOptionalMixin { self: ElementBase =>

  def minOccurs: Int
  def maxOccurs: Int

  final override lazy val isScalar = minOccurs == 1 && maxOccurs == 1

  /**
   * Distinguishes elements which have minOccurs 0, maxOccurs 1, and
   * a dfdl:occursCountKind that means parsing/unparsing will respect these
   * bounds.
   *
   * This enables implementations to use different, lighter weight, representations
   * for optional elements (e.g., null or not null object reference) vs what
   * is required for arrays (growable vector of slots).
   */
  final override lazy val isOptional = {
    (minOccurs, maxOccurs) match {
      case (1, 1) => false // scalars are not optional
      case (0, max) => {
        // now we must check on occursCountKind.
        // if parsed or stopValue then we consider it an array
        if (occursCountKind =:= OccursCountKind.Parsed ||
          occursCountKind =:= OccursCountKind.StopValue ||
          occursCountKind =:= OccursCountKind.Expression) {
          // we disregard the min/max occurs
          false
        } else {
          max match {
            case 1 => true
            case _ => false
          }
        }
      }
      case _ => false
    }
  }

  /**
   * True if the element is required to appear in the DFDL Infoset.
   *
   * This includes elements that have no representation in the
   * data stream. That is, an element with dfdl:inputValueCalc will be isRequired true.
   *
   * Takes into account that some dfdl:occursCountKind can make
   * seemingly required elements (based on minOccurs) optional or
   * repeating.
   */
  final override def isRequired: Boolean = LV('isRequired) {
    // TODO: Rename to isRequiredOrComputed
    val res = {
      if (isScalar) true
      else if (isOptional) false
      else if (isArray) isRequiredArrayElement
      else false
    }
    res
  }.value

  /**
   * True if a "real" array, i.e., not an optional element, but something
   * that can potentially have 2 or more occurrences based on maxOccurs
   * and dfdl:occursCountKind that indicates whether maxOccurs will be respected.
   */
  final override lazy val isArray = {
    if (isOptional) false
    else {
      val UNBOUNDED = -1
      (minOccurs, maxOccurs) match {
        case (1, 1) => false
        case (_, n) if n > 1 => true
        case (_, UNBOUNDED) => true
        /**
         * This next case is for occursCountKinds parsed and stopValue.
         * These only use min/maxOccurs for validation, so anything
         * with these occursCountKinds is an array (so long as it isn't
         * scalar)
         */
        case (_, 1) if (occursCountKind == OccursCountKind.Parsed ||
          occursCountKind == OccursCountKind.StopValue ||
          occursCountKind == OccursCountKind.Expression) => true
        case _ => false
      }
    }
  }

  /**
   * True if an array has at least one required element based
   * on a minOccurs and a dfdl:occursCountKind that means that
   * minOccurs will be respected.
   */
  final lazy val isRequiredArrayElement = {
    // TODO: rename to isArraywithAtLeastOneRequiredArrayElement
    isArray &&
      minOccurs > 0 &&
      (occursCountKind == OccursCountKind.Fixed ||
        occursCountKind == OccursCountKind.Implicit ||
        occursCountKind == OccursCountKind.Expression)
  }
}

// A Particle is something that can be repeating or optional.
trait ParticleMixin extends RequiredOptionalMixin { self: ElementBase =>

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
  
  /**
   * Can have a varying number of occurrences.
   */
  final override lazy val isVariableOccurrences = minOccurs != maxOccurs

  final lazy val isFixedOccurrences = {
    // TODO maybe do optimizations to take scope into consideration. E.g.,
    // We could be in a context where the value of our occursCount expression
    // will always be a constant.
    occursCountKind == OccursCountKind.Fixed
  }

  /**
   * Does this node have statically required instances in the data stream.
   *
   * This excludes elements that have no representation e.g., elements with dfdl:inputValueCalc.
   */
  final def hasStaticallyRequiredInstances = LV('hasStaticallyRequiredInstances) {
    // TODO: Rename to hasStaticallyRequiredOccurrencesInDataRepresentation
    val res =
      if (!isRepresented) false // if there's no rep, then it's not statically required.
      else if (isScalar) true
      else if (isFixedOccurrences) true
      else if (minOccurs > 0) true // FIXME: should call isRequiredArrayElement
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
