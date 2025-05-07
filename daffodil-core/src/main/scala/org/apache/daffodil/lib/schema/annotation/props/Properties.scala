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

import org.apache.daffodil.lib.cookers.Converter
import org.apache.daffodil.lib.exceptions._
import org.apache.daffodil.lib.util.Misc._
import org.apache.daffodil.lib.util._

/**
 * Enum class as basis for our DFDL properties
 *
 * This is the best Enum idiom I could find on StackOverflow and other Scala web sites.
 * (That was a somewhat dated comment. This Enum idiom is used in the DFDL properties
 * and is laid down by the code generator for properties. Hence, not changing it eventhough
 * in other places in our code we have Enum.scala which defines a slightly improved
 * enum idiom. Perhaps fix and make consistent both of these once Scala actually puts in
 * a decent not so clumsy enum idiom into the language.)
 *
 * An enumeration for a DFDL property is defined like this:
 * @example {{{
 * ////////////////////////////////////////////////////////////////////////
 * // <xsd:simpleType name="BinaryNumberRepEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
 * //     <xsd:restriction base="xsd:string">
 * //       <xsd:enumeration value="packed"></xsd:enumeration>
 * //       <xsd:enumeration value="bcd"></xsd:enumeration>
 * //       <xsd:enumeration value="binary"></xsd:enumeration>
 * //     </xsd:restriction>
 * //   </xsd:simpleType>
 *
 * sealed trait BinaryNumberRep extends BinaryNumberRep.Value
 * object BinaryNumberRep extends Enum[BinaryNumberRep] {
 *   case object Packed extends BinaryNumberRep
 *   case object Bcd extends BinaryNumberRep
 *   case object Binary extends BinaryNumberRep
 *   override lazy val values = Array(Packed, Bcd, Binary)
 *
 *   def apply(name: String) : BinaryNumberRep = stringToEnum("binaryNumberRep", name)
 * }
 *
 * trait BinaryNumberRepMixin extends PropertyMixin {
 *   //
 *   // lazy so if we don't need the property we don't access it. If we do access it, and it
 *   // is not found, it is a schema definition error.
 *   //
 *   lazy val binaryNumberRep = BinaryNumberRep(getProperty("binaryNumberRep"))
 *   ...other stuff...
 * }
 * }}}
 * The first bunch of // comments is the fragment of the DFDL Annotations schema which
 * defines the enumeration. The scala code to realize this follows (this is the output
 * from a code generator). This is just in here to serve as documentation.
 *
 * For the most as a programmer you don't have to deal with the above code much. To
 * use a property you mix in the trait BinaryNumberRepMixin into the class which is
 * to have access to the property. This is actually done with traits that group these individual enum
 * traits into the proper sets for the various schema components.
 *
 * Then within that class's code, to access the property value you just use the lazy val,
 * in this case binaryNumberRep. The type of which will be the enum, with values given by the
 * case objects BinaryNumberRep.Packed, BinaryNumberRep.Bcd, and BinaryNumberRep.Binary.
 *
 * Code using properties never deals with the strings for these enumerations,
 * and never deals with where/how to retrieve properties for any properties, enum or otherwise.
 * Also, retrieving properties is unconditional.
 * If you ask for a property value and it's not defined, it's a schema-definition error.
 * (This is part of the DFDL spec. Implementations are not to provide default values for properties.)
 */
abstract class EnumBase
abstract class EnumValueBase extends Serializable
abstract class Enum[A] extends EnumBase with Converter[String, A] {
  def toPropName(prop: A) = prop.toString

  def values: Array[A]

  /**
   * This is invoked at runtime to compare expression results to see if they
   * match the strings for the enum values. So this has to be fast.
   */
  def stringToEnum(enumTypeName: String, str: String, context: ThrowsSDE): A = {
    val opt = optionStringToEnum(enumTypeName, str)
    if (opt.isDefined) opt.get
    else {
      context.SDE("Unknown value for %s property: %s", enumTypeName, str)
    }
  }

  def optionStringToEnum(enumTypeName: String, str: String): Option[A] = {
    var i: Int = 0
    while (i < values.size) {
      if (values(i).toString.equals(str)) {
        return Some(values(i))
      }
      i += 1
    }
    None
  }

  override protected def convert(b: String, context: ThrowsSDE, forUnparse: Boolean): A =
    apply(b, context)

  def apply(name: String, context: ThrowsSDE): A
} // end class
trait EnumValue extends EnumValueBase {
  override lazy val toString = {
    val theVal = this
    val cn = getNameFromClass(this)
    val en = cn match {
      //
      // Special case for CalendarFirstDayOfWeek
      //
      case "Sunday" | "Monday" | "Tuesday" | "Wednesday" | "Thursday" | "Friday" | "Saturday" =>
        cn
      case _ => Misc.toInitialLowerCaseUnlessAllUpperCase(cn)
    }
    en
  }
}

/**
 * base mixin for traits representing collections of DFDL properties
 *
 */
trait PropertyMixin extends FindPropertyMixin with ThrowsSDE with SavesErrorsAndWarnings {

  /**
   * Properties will push their toString function onto this list
   *
   * Each object that has any properties mixed in must implement this
   * value.
   */
  var toStringFunctionList: List[() => String] = Nil

  /**
   * prints all the properties on the object.
   */
  def verboseToString = {
    val className = getNameFromClass(this)
    val props = toStringFunctionList
      .map { f => f.apply() }
      .foldLeft(
        className +
          "("
      )(_ + _)
    val suffix = ")"
    val str = props + suffix
    str
  }

  def registerToStringFunction(f: (() => String)): Unit = {
    toStringFunctionList +:= f
  }

  /**
   * Convert a property string value to a Boolean
   *
   * Note: no error checking is required, as we assume the schema has already been
   * validated, so we only have to deal with the valid formats for Boolean.
   */
  def convertToBoolean(pv: String) = {
    pv match {
      case "true" => true
      case "false" => false
      case "TRUE" => true
      case "FALSE" => false
    }
  }

  /**
   * Convert a property string value to a Int
   *
   * Note: no error checking is required, as we assume the schema has already been
   * validated, so we only have to deal with the valid formats for Int.
   */
  def convertToInt(pv: String) = {
    pv.toInt
  }

  def convertToDouble(pv: String) = {
    pv.toDouble
  }

  def convertToFloat(pv: String) = {
    pv.toFloat
  }

  /**
   * There's no conversion to do here, but to eliminate a special case in the code generator
   * we always generate a call to a convertToTYPE function.
   */
  def convertToString(pv: String) = {
    // TODO DFDL String Literal processing to deal with
    // entities and raw bytes
    pv
  }

  def convertToQName(pv: String, pl: LookupLocation) = {
    pl.resolveQName(pv)
  }

  def convertToNCName(pv: String): String = {
    // remember: schema validation already checked format of NCName for us.
    pv
  }

} // end trait
