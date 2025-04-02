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

package org.apache.daffodil.lib.schema.annotation.props

import org.apache.daffodil.lib.iapi.LocationInSchemaFile
import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.DPathUtil
import org.apache.daffodil.lib.xml.ResolvesQNames

/**
 * This file is classes and traits to implement
 * property lookups, diagnostics about them, and the
 * lexical scoping of properties from the schema-document level.
 *
 * The ultimate data structure is two sequences, one for nonDefault,
 * one for default. Each such sequence is a sequence of ChainRropProviders.
 * Each of those is a sequence of LeafPropProviders
 */

/**
 * Result of searching for a property.
 *
 * Includes location information so you can issue a diagnostic
 * about where a conflicting property
 * definition was found, or all the places where one searched
 * but did not find a property definition.
 *
 * One must also have the location object because it must
 * be used if the property value contains a QName (or is an expression
 * containing QNames). That is, you can't interpret a QName without
 * the scope from the XML where it was written.
 */
sealed abstract class PropertyLookupResult(val pname: String) extends Serializable {
  def isDefined: Boolean

  /**
   * True if the property value was found by way of the default format, that is
   * the lexically enclosing format properties specified by the dfdl:format annotation
   * for the schema file.
   */
  def isFromDefaultFormat: Boolean

  lazy val toOption = this match {
    case Found(value, _, _, _) => Some(value)
    case _: NotFound => None
  }
}

/**
 * Represents a successful property lookup result.
 *
 * @constructor Create a new Found instance with the specified value, location, property name, and format information.
 * @param value               The value associated with the property.
 * @param location            The lookup location where the property was found.
 * @param pname               The name of the property.
 * @param isFromDefaultFormat Indicates whether the property was found in the default format.
 */
case class Found(
  value: String,
  location: LookupLocation,
  override val pname: String,
  override val isFromDefaultFormat: Boolean
) extends PropertyLookupResult(pname) {
  override def isDefined = true
}

/**
 * Represents a property lookup result when the property is not found.
 *
 * @param localWhereLooked   The sequence of lookup locations where the property was searched locally.
 * @param defaultWhereLooked The sequence of lookup locations where the property was searched in the default format.
 * @param pname              The name of the property that was searched.
 */
case class NotFound(
  localWhereLooked: Seq[LookupLocation],
  defaultWhereLooked: Seq[LookupLocation],
  override val pname: String
) extends PropertyLookupResult(pname) {
  override def isDefined = false
  // $COVERAGE-OFF$
  override def isFromDefaultFormat: Boolean = Assert.usageError("Not meaningful for NotFound.")
  // $COVERAGE-ON$
}

/**
 * A lookup location is where we indicate a property binding
 * resides. This can be an annotation object (ex: a dfdl:sequence or a dfdl:format)
 * or in the case of short-form properties it could be the
 * annotated schema component itself.
 *
 * The point is to get the file and line number information right
 * so we point the user at the right place.
 *
 * It also resolves QNames in the right way. So given a property value that
 * contains a QName, the associated LookupLocation can be used to resolve that
 * QName to a namespace and a local name.
 */
trait LookupLocation extends ResolvesQNames with LocationInSchemaFile

trait PropTypes {

  /**
   * type of a map entry which maps a property name as key, to a property value,
   * and along side it is a LookupLocation object telling us where we found that
   * property value.
   *
   * The property maps/lists in DFDLFormatAnnotation have been enhanced to have
   * this LookupLocation thing along their side to allow issuing
   * better diagnostic messages about conflicting property definitions
   */
  type PropItem = (String, (String, LookupLocation))

  type PropMap = Map[String, (String, LookupLocation)]

  val emptyPropMap = Map.empty.asInstanceOf[PropMap]
}

trait FindPropertyMixin extends PropTypes {

  /**
   * Implemented by users of the mixin so that we can
   * report a property not found error.
   */
  def SDE(str: String, args: Any*): Nothing

  /**
   * Implemented by users of the mixin so that we can
   * report property value warnings.
   */
  def SDW(warnID: WarnID, std: String, args: Any*): Unit

  /**
   * Implemented in various ways by users of the mixin to find a property. This
   * should never be directly called to find a property. Instead one should use
   * findPropertyOption and friends, which ensure that caching is consistent.
   */
  protected def lookupProperty(pname: String): PropertyLookupResult

  val propCache = new scala.collection.mutable.LinkedHashMap[String, PropertyLookupResult]

  /**
   * expressionAllowed describes whether a run-time expression
   * is valid for this option. Used to decide whether a warning
   * should be produced if the value "looks like" an expression
   */
  def findPropertyOption(
    pname: String,
    expressionAllowed: Boolean = false
  ): PropertyLookupResult = {
    val propCacheResult = propCache.get(pname)
    val propRes =
      propCacheResult match {
        case Some(res) => res
        case None => {
          val lr = lookupProperty(pname)
          propCache.put(pname, lr)
          lr
        }
      }
    // if this looks like it could be an expression, issue a warning
    // but only if expressionAllowed is false
    if (!expressionAllowed) {
      propRes match {
        case Found(v, _, _, _) => {
          if (DPathUtil.isExpression(v)) {
            SDW(
              WarnID.NonExpressionPropertyValueLooksLikeExpression,
              "Property %s looks like an expression but cannot be an expression: %s",
              pname,
              v
            )
          }
        }
        case _ => {
          // Do Nothing
        }
      }
    }
    propRes
  }

  /**
   * Call this to find/get a property.
   *
   * Property values are non-optional in DFDL. If they're not
   * there but a format requires them, then it's always an error.
   *
   * Note also that DFDL doesn't have default values for properties. That means
   * that most use of properties is unconditional. Get the property value, and
   * it must be there, or its an error. There are very few exceptions to this
   * rule.
   */
  final def findProperty(pname: String): Found = {
    val prop = findPropertyOption(pname)
    requireProperty(prop)
  }

  final def requireProperty(prop: PropertyLookupResult) = {
    val res = prop match {
      case f: Found => f
      //
      // TODO: Internationalization - should not be assembling error messages in English like this.
      // All this has to be delegated to a layer that uses the english string as a key to find
      // the translation.
      //
      // Hence, we need a way to explicitly get the possibly translated version of a
      // literal english string when that string is not the direct argument of a SDE call.
      //
      case nf: NotFound => requiredButNotFound(nf)
    }
    res
  }

  private def requiredButNotFound(nf: NotFound) = {
    val NotFound(nonDefaultLocs, defaultLocs, pname) = nf
    val ndListText = nonDefaultLocs.map { _.locationDescription }.mkString("\n")
    val dListText = defaultLocs.map { _.locationDescription }.mkString("\n")
    val nonDefDescription =
      if (nonDefaultLocs.length > 0)
        "\nNon-default properties were combined from these locations:\n" + ndListText + "\n"
      else ""
    val defLocsDescription =
      if (defaultLocs.length > 0)
        "\nDefault properties were taken from these locations:\n" + dListText + "\n"
      else "\nThere were no default properties.\n"
    SDE("Property %s is not defined.%s%s", pname, nonDefDescription, defLocsDescription)
  }

  /**
   * It is ok to use getProperty if the resulting property value cannot ever contain
   * a QName that would have to be resolved.
   */
  final def getProperty(pname: String): String = {
    val Found(res, _, _, _) = findProperty(pname)
    res
  }

  /**
   * Don't use this much. If an SDE needs to be reported, you won't have
   * the source of the property to put into the message. Use findPropertyOption
   * instead. That returns the value and the LookupLocation where it was
   * found for use in diagnostics.
   *
   * Also, don't use if the property value could ever contain a QName, because
   * one needs that LookupLocation to resolve QNames properly.
   *
   * Note: any expression can contain QNames, so no expression-valued property or
   * one that could be a value or an expression, should ever use this or getProperty.
   *
   * See JIRA DFDL-506.
   */
  final def getPropertyOption(
    pname: String,
    expressionAllowed: Boolean = false
  ): Option[String] = {
    val lookupRes = findPropertyOption(pname, expressionAllowed)
    val res = lookupRes match {
      case Found(v, _, _, _) => Some(v)
      case _ => None
    }
    res
  }

  /**
   * For unit testing convenience, or for use when debugging.
   */
  def verifyPropValue(key: String, value: String): Boolean = {
    val prop = findPropertyOption(key)
    prop match {
      case Found(v, _, _, _) if (v == value) => true
      case _: Found => false
      case _: NotFound => false
    }
  }
}
