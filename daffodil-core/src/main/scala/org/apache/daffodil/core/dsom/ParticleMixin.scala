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

package org.apache.daffodil.core.dsom

import org.apache.daffodil.lib.cookers.OccursStopValueCooker
import org.apache.daffodil.lib.equality.*
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.*
import org.apache.daffodil.runtime1.dpath.InvalidPrimitiveDataException
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive

trait RequiredOptionalMixin { self: ElementBase =>
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
        if (
          occursCountKind =:= OccursCountKind.Parsed ||
          occursCountKind =:= OccursCountKind.StopValue ||
          occursCountKind =:= OccursCountKind.Expression
        ) {
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
   * For an array or optional, defined if there is a PoU for the element,
   * the defined value being the effective minOccurs value.
   */
  final lazy val optPoUMinOccurs: Option[Int] = {
    if (isScalar) None
    else {
      occursCountKind match {
        case OccursCountKind.Parsed => Some(0)
        case OccursCountKind.Fixed => None
        case OccursCountKind.Expression => None
        case OccursCountKind.Implicit => Some(minOccurs)
        case OccursCountKind.StopValue => None
      }
    }
  }

  /**
   * True if the element is required to appear in the stream of unparser infoset events.
   * False means appears "maybe", may or may not appear.
   *
   * This includes elements that have no representation in the
   * data stream. That is, an element with dfdl:inputValueCalc will be isRequiredStreamingUnparserEvent true.
   *
   * All optionals are treated as not required because we tolerate invalidity of the
   * data here.
   *
   * OutputValueCalc elements are treated as optional. If present they are supposed to get their
   * values ignored and overwritten by the computation.
   */
  final lazy val isRequiredStreamingUnparserEvent: Boolean = {
    val res = {
      if (isScalar) !this.isOutputValueCalc
      else if (isOptional) false
      // Arrays with minOccurs = 0 are not required to be in the infoset.
      // Arrays that have at least one required element (i.e. minOccurs > 0) are required to
      // have unparser events. Otherwise we could unparse malformed data. The exception to this
      // is required arrays that are defaultable (which is not yet implemented, so will never be true
      // at this point but will produce a subset error so people don't depend on broken/nyi behavior).
      else if (isArray && isDefaultable) false
      else if (isArrayWithAtLeastOneRequiredArrayElement) true
      else if (isArray) false
      else if (minOccurs == 0 && maxOccurs == 0) false
      else {
        Assert.invariant(this.isOutputValueCalc)
        false
      }
    }
    res
  }

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
        case (_, 1) | (0, 0)
            if (occursCountKind == OccursCountKind.Parsed ||
              occursCountKind == OccursCountKind.StopValue ||
              occursCountKind == OccursCountKind.Expression) =>
          true
        /**
         * Special case for minoccurs 0 and maxOccurs 0 when OCK is 'implicit' in that
         * case we treat as an array that cannot have any occurrences.
         */
        case (0, 0) if (occursCountKind == OccursCountKind.Implicit) => true
        case _ => false
      }
    }
  }

  /**
   * True if an array has at least one required element based
   * on a minOccurs and a dfdl:occursCountKind that means that
   * minOccurs will be respected.
   */
  final lazy val isArrayWithAtLeastOneRequiredArrayElement = {
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
   *
   * Can be used for any Term, e.g., returns false for model groups
   * true for elements with potentially varying number of occurrences.
   *
   * Scalars are false here, as are fixed-length arrays.
   *
   * Differs from (isOptional || isArray) because that is true for fixed-length
   * arrays, where this is false if they are fixed length.
   *
   * dfdl:occursCountKind is not part of this concept. So for example
   * an element with occursCountKind='expression' has exactly the number
   * of occurrences the expression value specifies. But this is considered varying as
   * that element declaration may correspond to many arrays of data where
   * the expression gives a different number of occurrences each time.
   */
  final override lazy val isVariableOccurrences = minOccurs != maxOccurs

  /**
   * True if a recurring element has a fixed number of occurrences.
   *
   * One way this can happen is dfdl:occursCountKind of 'fixed'.
   *
   * In the future, can be enhanced to take more situations into account.
   * E.g., Another way something can be fixed number of occurrences
   * is dfdl:occursCountKind='expression' where the expression is a constant,
   * whether a manifest constant like '5', or an expression that happens to be
   * shown, by the schema compiler, to be constant for this element. One such example
   * would be if the expression is just the value of a variable for which there are
   * no dfdl:setVariable statements possible, so it is guaranteed to have the default or
   * an externally specified fixed number as its value.
   */
  final lazy val isFixedOccurrences = {
    Assert.usage(!isScalar)
    occursCountKind == OccursCountKind.Fixed
  }

  /**
   * True if this term has statically required instances in the data stream.
   *
   * This excludes elements that have no representation e.g., elements with dfdl:inputValueCalc.
   */
  final lazy val hasStaticallyRequiredOccurrencesInDataRepresentation =
    LV(Symbol("hasStaticallyRequiredOccurrencesInDataRepresentation")) {
      val res =
        if (!isRepresented) false // if there's no rep, then it's not statically required.
        else if (isScalar) true
        else if (isFixedOccurrences) true
        else if (isArrayWithAtLeastOneRequiredArrayElement) true
        else false
      res
    }.value

  /**
   * True if this element uses occursCountKind='stopValue' to determine the end of its
   * occurrences. (For scalar elements the occursCountKind is ignored, so this is false
   * for them.)
   */
  final lazy val hasStopValue =
    !isScalar && occursCountKind == OccursCountKind.StopValue

  /**
   * The dfdl:occursStopValue property value, cooked (i.e. split into a list of string
   * literals with DFDL entities like %ES; replaced) into individual stop values.
   *
   * Only meaningful (and only accessed) when hasStopValue is true.
   */
  final lazy val occursStopValues: Seq[String] = {
    Assert.usage(hasStopValue)
    val maybePropValue = findPropertyOption("occursStopValue").toOption
    schemaDefinitionWhen(
      maybePropValue.isEmpty,
      "Property occursCountKind='stopValue' requires the dfdl:occursStopValue property to be defined."
    )
    // the cooker SDEs if the cooked list of stop values is empty
    OccursStopValueCooker.convertConstant(maybePropValue.get, this, forUnparse = false)
  }

  /**
   * The stop values converted to values of this element's simple type. Comparison with
   * parsed values is by value (e.g. data 00010 matches a stop value of 10 for xs:int),
   * as required by the DFDL specification (the stop value must be a value in the domain
   * of the declared simple type of the data item).
   */
  final lazy val occursStopValuesForParse: Seq[DataValuePrimitive] = {
    schemaDefinitionWhen(
      !isSimpleType,
      "Property occursCountKind='stopValue' requires a simple type. The value of each occurrence is compared with the dfdl:occursStopValue."
    )
    occursStopValues.map { sv =>
      try {
        primType.fromXMLString(sv)
      } catch {
        case ipd: InvalidPrimitiveDataException =>
          SDE("Invalid dfdl:occursStopValue: %s", ipd.getMessage)
      }
    }
  }

  /**
   * Checks that an element using occursCountKind='stopValue' is valid. Forced by
   * ElementBaseGrammarMixin, so it runs whenever such an element is compiled.
   */
  final lazy val checkOccursStopValue: Unit = {
    if (hasStopValue) {
      schemaDefinitionWhen(
        !isSimpleType,
        "Property occursCountKind='stopValue' requires a simple type. The value of each occurrence is compared with the dfdl:occursStopValue."
      )
      schemaDefinitionWhen(
        !isRepresented,
        "Property occursCountKind='stopValue' is not meaningful for elements with no data representation (e.g. dfdl:inputValueCalc)."
      )
      schemaDefinitionWhen(
        isNillable,
        "Property occursCountKind='stopValue' is not supported for nillable elements."
      )
      // force the checks (present, non-empty, valid for the simple type) of the stop
      // values themselves
      occursStopValuesForParse
    }
  }
}
