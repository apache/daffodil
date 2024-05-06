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

package org.apache.daffodil.runtime1.dpath

import org.apache.daffodil.lib.calendar.DFDLCalendarConversion
import org.apache.daffodil.lib.cookers.EntityReplacer
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.runtime1.dsom.SchemaDefinitionError
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueBool
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueShort
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueString

case class DFDLCheckConstraints(recipe: CompiledDPath) extends RecipeOpWithSubRecipes(recipe) {
  override def run(dstate: DState): Unit = {
    recipe.run(dstate)
    if (dstate.currentElement.valid.isDefined) {
      dstate.setCurrentValue(dstate.currentElement.valid.get)
    } else {
      val res = DFDLCheckConstraintsFunction.executeCheck(dstate.currentSimple).isOK
      dstate.currentElement.setValid(res)
      dstate.setCurrentValue(res)
    }
  }
}

case class DFDLCheckRange(
  dataRecipe: CompiledDPath,
  minRecipe: CompiledDPath,
  maxRecipe: CompiledDPath,
  compare: CompareOpBase
) extends RecipeOpWithSubRecipes(dataRecipe, minRecipe, maxRecipe) {
  override def run(dstate: DState): Unit = {
    val saved = dstate.currentNode
    dataRecipe.run(dstate)
    val dataVal = dstate.currentValue.getNonNullable
    dstate.setCurrentNode(saved)
    minRecipe.run(dstate)
    val minVal = dstate.currentValue.getNonNullable
    dstate.setCurrentNode(saved)
    maxRecipe.run(dstate)
    val maxVal = dstate.currentValue.getNonNullable

    val res =
      compare.operate(minVal, dataVal).getBoolean && compare.operate(dataVal, maxVal).getBoolean
    dstate.setCurrentValue(res)
  }
}

case class DFDLDecodeDFDLEntities(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  override def computeValue(str: DataValuePrimitive, dstate: DState): DataValueString = {
    val dfdlString = EntityReplacer { _.replaceAll(str.getString, None) }
    dfdlString
  }
}

case class DFDLEncodeDFDLEntities(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  override def computeValue(str: DataValuePrimitive, dstate: DState): DataValueString =
    constructLiteral(str.getString)

  def constructLiteral(s: String) = {
    val sb = new StringBuilder
    s.foreach(c => {
      c match {
        case '%' => sb.append("%%") // \u0025
        case '\u0000' | 0xe000 => sb.append("%NUL;")
        case '\u0001' | 0xe001 => sb.append("%SOH;")
        case '\u0002' | 0xe002 => sb.append("%STX;")
        case '\u0003' | 0xe003 => sb.append("%ETX;")
        case '\u0004' | 0xe004 => sb.append("%EOT;")
        case '\u0005' | 0xe005 => sb.append("%ENQ;")
        case '\u0006' | 0xe006 => sb.append("%ACK;")
        case '\u0007' | 0xe007 => sb.append("%BEL;")
        case '\u0008' | 0xe008 => sb.append("%BS;")
        case '\u0009' => sb.append("%HT;") // OK, not remapped
        case '\u000A' => sb.append("%LF;") // OK, not remapped
        case '\u000B' | 0xe00b => sb.append("%VT;")
        case '\u000C' | 0xe00c => sb.append("%FF;")
        case '\u000D' => sb.append("%CR;") // OK, not remapped
        case '\u000E' | 0xe00e => sb.append("%SO;")
        case '\u000F' | 0xe00f => sb.append("%SI;")
        case '\u0010' | 0xe010 => sb.append("%DLE;")
        case '\u0011' | 0xe011 => sb.append("%DC1;")
        case '\u0012' | 0xe012 => sb.append("%DC2;")
        case '\u0013' | 0xe013 => sb.append("%DC3;")
        case '\u0014' | 0xe014 => sb.append("%DC4;")
        case '\u0015' | 0xe015 => sb.append("%NAK;")
        case '\u0016' | 0xe016 => sb.append("%SYN;")
        case '\u0017' | 0xe017 => sb.append("%ETB;")
        case '\u0018' | 0xe018 => sb.append("%CAN;")
        case '\u0019' | 0xe019 => sb.append("%EM;") // and above remapped to c + 0xE000
        case '\u001A' => sb.append("%SUB;")
        case '\u001B' => sb.append("%ESC;")
        case '\u001C' => sb.append("%FS;")
        case '\u001D' => sb.append("%GS;")
        case '\u001E' => sb.append("%RS;")
        case '\u001F' => sb.append("%US;")
        case '\u0020' => sb.append("%SP;")
        case '\u007F' => sb.append("%DEL;")
        case '\u00A0' => sb.append("%NBSP;")
        case '\u0085' => sb.append("%NEL;")
        case '\u2028' => sb.append("%LS;")
        case _ => sb.append(c)
      }
    })
    sb.toString()
  }
}

case class DFDLContainsDFDLEntities(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  override def computeValue(str: DataValuePrimitive, dstate: DState): DataValueBool =
    EntityReplacer { x => x.hasDfdlEntity(str.getString) }
}

/**
 * Returns the timezone component, if any, of \$arg as an xs:string.  The \$arg
 * is of type xs:dateTime, xs:date, or xs:time.
 *
 * If \$arg has a timezone component, then the result is a string in the
 * format of an ISO Time zone designator.  Interpreted as an offset from UTC,
 * its value may range from +14:00 to -14:00 hours, both inclusive.  The UTC
 * time zone is represented as "+00:00".  If the \$arg has no timezone
 * component, then "" (empty string) is returned.
 */
case class DFDLTimeZoneFromDFDLCalendar(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {

  override def computeValue(value: DataValuePrimitive, dstate: DState): DataValueString = {
    val dfdlcal = value.getCalendar
    DFDLCalendarConversion.timeZonePartToXMLString(dfdlcal)
  }
}

case class DFDLTestBit(dataRecipe: CompiledDPath, bitPos1bRecipe: CompiledDPath)
  extends RecipeOpWithSubRecipes(dataRecipe, bitPos1bRecipe) {

  override def run(dstate: DState): Unit = {
    val saved = dstate.currentNode
    dataRecipe.run(dstate)
    val dataVal = dstate.intValue
    dstate.setCurrentNode(saved)
    bitPos1bRecipe.run(dstate)
    val bitPos1b = dstate.intValue
    checkRange(bitPos1b)
    val res = testBit(dataVal, bitPos1b)
    dstate.setCurrentValue(res)
  }

  private def checkRange(i: Int) = {
    if (i > 8 || i < 1) {
      throw new SchemaDefinitionError(
        None,
        None,
        "dfdl:testBit $bitPos must be between 1 and 8 (inclusive). Was %s.",
        i
      )
    }
  }

  private def testBit(data: Int, bitPos1b: Int): Boolean = {
    // Assume 8-bit
    val shifted = data >>> (bitPos1b - 1)
    val maskedVal = shifted & 1
    if (maskedVal == 1) true
    else false
  }
}

case class DFDLSetBits(bitRecipes: List[CompiledDPath])
  extends RecipeOpWithSubRecipes(bitRecipes) {

  override def run(dstate: DState): Unit = {
    var byteVal: Int = 0

    Assert.invariant(bitRecipes.length == 8)
    val saved = dstate.currentNode
    var i = 0
    var bitR = bitRecipes
    while (i < 8) {
      val br = bitR.head
      dstate.setCurrentNode(saved)
      br.run(dstate)
      val currentVal = dstate.intValue
      if (processValue(currentVal)) {
        byteVal |= 1 << i
      }
      i += 1
      bitR = bitR.tail
    }
    val res: DataValueShort = byteVal.toShort
    dstate.setCurrentValue(res)
  }

  private def processValue(i: Int): Boolean = {
    if (i < 0 || i > 1)
      throw new IllegalArgumentException(
        "dfdl:setBits arguments must each be 0 or 1, but value was: %s.".format(i)
      )
    if (i == 0) false
    else true
  }
}
