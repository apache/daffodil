package edu.illinois.ncsa.daffodil.schema.annotation.props

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

// Copyright (C) 2012, Michael J. Beckerle. All Rights Reserved.

import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.util.Misc._
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.Info
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.dsom.FindPropertyMixin

/**
 * Enum class as basis for our DFDL properties
 *
 * This is the best Enum idiom I could find on StackOverflow and other Scala web sites.
 *
 * An enumeration for a DFDL property is defined like this:
 * @example {{{
 * ////////////////////////////////////////////////////////////////////////
 * // <xsd:simpleType name="BinaryNumberRepEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
 * // 		<xsd:restriction base="xsd:string">
 * // 			<xsd:enumeration value="packed"></xsd:enumeration>
 * // 			<xsd:enumeration value="bcd"></xsd:enumeration>
 * // 			<xsd:enumeration value="binary"></xsd:enumeration>
 * // 		</xsd:restriction>
 * // 	</xsd:simpleType>
 *
 * sealed trait BinaryNumberRep extends BinaryNumberRep.Value
 * object BinaryNumberRep extends Enum[BinaryNumberRep] {
 *   case object Packed extends BinaryNumberRep ; forceConstruction(Packed)
 *   case object Bcd extends BinaryNumberRep ; forceConstruction(Bcd)
 *   case object Binary extends BinaryNumberRep ; forceConstruction(Binary)
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
abstract class EnumValueBase
abstract class Enum[A] extends EnumBase {
  class Value extends EnumValueBase { self: A => {
      val theVal = this
      _values :+= theVal
      _values
    }
  }

  def toPropName(prop: A) = {
    val s = prop.toString
    val capS = s.substring(0, 1).toLowerCase()
    val propName = capS + s.substring(1)
    propName
  }

  private var _values = List.empty[A]
  def stringToEnum(enumTypeName: String, str: String, context: ThrowsSDE) = {
    val opt = _values.find(_.toString.toLowerCase() == str.toLowerCase)
    opt match {
      case Some(e) => e
      case None => context.SDE("Unknown property value ", enumTypeName, str)
    }
  }
  /**
   * Useful for diagnostic messages where you want to say "must be one of ...." and list the possibilities.
   */
  def allValues = _values

  /**
   * Scala delays construction of case objects (presumably because many programs don't use them at all)
   * We need to force creation of our inner property case objects because constructing them also has
   * the side-effect of registering them in the _values list.
   */
  def forceConstruction(obj: Any) {
    //Assert.invariant(obj.toString() != "") // TODO: is forceConstruction needed at all?
  }
} // end class

/**
 * base mixin for traits representing collections of DFDL properties
 *
 */
trait PropertyMixin extends FindPropertyMixin with ThrowsSDE with Logging {

//  /**
//   * Only for testing purposes
//   */
//  def properties: PropMap

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
    val props = toStringFunctionList.map { f => f.apply() }.foldLeft(className +
      "(")(_ + _)
    val suffix = ")"
    val str = props + suffix
    str
  }

  def registerToStringFunction(f: (() => String)) {
    toStringFunctionList = toStringFunctionList :+ f
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

  def convertToQName(pv: String): String = {
    // remember: schema validation already checked format of QName for us.
    pv
  }

  def convertToNCName(pv: String): String = {
    // remember: schema validation already checked format of NCName for us.
    pv
  }

} // end trait

