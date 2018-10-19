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

package org.apache.daffodil.api

import org.apache.daffodil.exceptions.ThrowsSDE
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.schema.annotation.props.{ Enum => PropsEnum }

sealed trait WarnID extends WarnID.Value

/**
 * Warning Identifiers for use in suppressing warning messages that are
 * unavoidable and not helpful.
 *
 * TODO: This file really should be generated from dafext.xsd's
 * enumerations for the XML syntax that specifies these suppressions in
 * config files. As this list grows, it's more and more unwieldy.
 */
object WarnID extends PropsEnum[WarnID] {
  /**
   * Suppresses All Warnings
   */
  case object All extends WarnID; forceConstruction(All)

  /**
   * For selective suppression of individual warnings.
   *
   * Please add additional, preserving alphabetical order and copying the
   * apparent naming conventions.
   */
  case object AlignmentNotSame extends WarnID; forceConstruction(AlignmentNotSame)
  case object AppinfoDFDLSourceWrong extends WarnID; forceConstruction(AppinfoDFDLSourceWrong)
  case object AppinfoNoSource extends WarnID; forceConstruction(AppinfoNoSource)
  case object ChoiceInsideHiddenGroup extends WarnID; forceConstruction(ChoiceInsideHiddenGroup)

  /**
   * For DeprecatedThing warning suppression
   */
  case object DeprecatedBuiltInFormats extends WarnID; forceConstruction(DeprecatedBuiltInFormats)
  case object DeprecatedEncodingNameUSASCII7BitPacked extends WarnID; forceConstruction(DeprecatedEncodingNameUSASCII7BitPacked)
  case object DeprecatedFunctionDAFError extends WarnID; forceConstruction(DeprecatedFunctionDAFError)

  /**
   * Deprecated properties should all begin with "DeprecatedProperty..."
   */
  case object DeprecatedPropertySeparatorPolicy extends WarnID; forceConstruction(DeprecatedPropertySeparatorPolicy)
  case object EncodingErrorPolicyError extends WarnID; forceConstruction(EncodingErrorPolicyError)
  case object EscapeSchemeRefUndefined extends WarnID; forceConstruction(EscapeSchemeRefUndefined)
  case object FacetExplicitLengthOutOfRange extends WarnID; forceConstruction(FacetExplicitLengthOutOfRange)
  case object InconsistentLengthKind extends WarnID; forceConstruction(InconsistentLengthKind)
  case object IgnoreImport extends WarnID; forceConstruction(IgnoreImport)
  case object MultipleChoiceBranches extends WarnID; forceConstruction(MultipleChoiceBranches)
  case object NamespaceDifferencesOnly extends WarnID; forceConstruction(NamespaceDifferencesOnly)
  case object NoEmptyDefault extends WarnID; forceConstruction(NoEmptyDefault)
  case object PathNotToArray extends WarnID; forceConstruction(PathNotToArray)
  case object PatternEncodingSlashW extends WarnID; forceConstruction(PatternEncodingSlashW)
  case object QueryStylePathExpression extends WarnID; forceConstruction(QueryStylePathExpression)
  case object RegexPatternZeroLength extends WarnID; forceConstruction(RegexPatternZeroLength)

  /**
   * UnsupportedThing warning suppression.
   */
  case object UnsupportedAttributeBlockDefault extends WarnID; forceConstruction(UnsupportedAttributeBlockDefault)
  case object UnsupportedAttributeFinalDefault extends WarnID; forceConstruction(UnsupportedAttributeFinalDefault)
  case object UnsupportedAttributeSchemaLocation extends WarnID; forceConstruction(UnsupportedAttributeSchemaLocation)
  case object UnsupportedAttributeFormDefault extends WarnID; forceConstruction(UnsupportedAttributeFormDefault)

  /**
    * textOutputMinLength checking
    */
  case object TextOutputMinLengthOutOfRange extends WarnID; forceConstruction(TextOutputMinLengthOutOfRange)

  override def apply(name: String, context: ThrowsSDE) = Assert.usageError("not to be called. Call find(name) method instead.")

  def find(name: String): Option[WarnID] = optionStringToEnum("warning identifier", name)
}
