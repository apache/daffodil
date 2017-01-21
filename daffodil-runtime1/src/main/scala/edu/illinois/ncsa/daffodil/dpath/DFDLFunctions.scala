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

package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.calendar.DFDLCalendar
import edu.illinois.ncsa.daffodil.util.Numbers._
import edu.illinois.ncsa.daffodil.cookers.EntityReplacer

case class DFDLCheckConstraints(recipe: CompiledDPath) extends RecipeOpWithSubRecipes(recipe) {
  override def run(dstate: DState) {
    recipe.run(dstate)
    if (dstate.currentElement.valid.isDefined) {
      dstate.setCurrentValue(dstate.currentElement.valid.get)
    } else {
      val res = DFDLCheckConstraintsFunction.executeCheck(dstate.currentSimple) match {
        case Right(boolVal) => true
        case Left(msg) => false
      }
      dstate.currentElement.setValid(res)
      dstate.setCurrentValue(res)
    }
  }
}

case class DFDLDecodeDFDLEntities(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: AnyRef, dstate: DState) = {
    val dfdlString = EntityReplacer { _.replaceAll(str.asInstanceOf[String], None) }
    dfdlString
  }
}

case class DFDLEncodeDFDLEntities(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: AnyRef, dstate: DState) = constructLiteral(str.asInstanceOf[String])

  def constructLiteral(s: String) = {
    val sb = new StringBuilder
    s.foreach(c => {
      c match {
        case '%' => sb.append("%%") // \u0025
        case '\u0000' | 0xE000 => sb.append("%NUL;")
        case '\u0001' | 0xE001 => sb.append("%SOH;")
        case '\u0002' | 0xE002 => sb.append("%STX;")
        case '\u0003' | 0xE003 => sb.append("%ETX;")
        case '\u0004' | 0xE004 => sb.append("%EOT;")
        case '\u0005' | 0xE005 => sb.append("%ENQ;")
        case '\u0006' | 0xE006 => sb.append("%ACK;")
        case '\u0007' | 0xE007 => sb.append("%BEL;")
        case '\u0008' | 0xE008 => sb.append("%BS;")
        case '\u0009' => sb.append("%HT;") // OK, not remapped
        case '\u000A' => sb.append("%LF;") // OK, not remapped
        case '\u000B' | 0xE00B => sb.append("%VT;")
        case '\u000C' | 0xE00C => sb.append("%FF;")
        case '\u000D' => sb.append("%CR;") // OK, not remapped
        case '\u000E' | 0xE00E => sb.append("%SO;")
        case '\u000F' | 0xE00F => sb.append("%SI;")
        case '\u0010' | 0xE010 => sb.append("%DLE;")
        case '\u0011' | 0xE011 => sb.append("%DC1;")
        case '\u0012' | 0xE012 => sb.append("%DC2;")
        case '\u0013' | 0xE013 => sb.append("%DC3;")
        case '\u0014' | 0xE014 => sb.append("%DC4;")
        case '\u0015' | 0xE015 => sb.append("%NAK;")
        case '\u0016' | 0xE016 => sb.append("%SYN;")
        case '\u0017' | 0xE017 => sb.append("%ETB;")
        case '\u0018' | 0xE018 => sb.append("%CAN;")
        case '\u0019' | 0xE019 => sb.append("%EM;") // and above remapped to c + 0xE000
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

case class DFDLContainsDFDLEntities(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: AnyRef, dstate: DState) =
    EntityReplacer { x => asAnyRef(x.hasDfdlEntity(str.asInstanceOf[String])) }
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

  override def computeValue(value: AnyRef, dstate: DState) = {
    val calendar = value.asInstanceOf[DFDLCalendar]

    val res = if (calendar.hasTimeZone) { calendar.getTimeZoneString } else { "" }
    res
  }
}

case class DFDLTestBit(dataRecipe: CompiledDPath, bitPos1bRecipe: CompiledDPath)
  extends RecipeOpWithSubRecipes(dataRecipe, bitPos1bRecipe) {

  override def run(dstate: DState) {
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
      throw new SchemaDefinitionError(None, None,
        "dfdl:testBit $bitPos must be between 1 and 8 (inclusive). Was %s.", i)
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

case class DFDLSetBits(bitRecipes: List[CompiledDPath]) extends RecipeOpWithSubRecipes(bitRecipes) {

  override def run(dstate: DState) {
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
    dstate.setCurrentValue(byteVal)
  }

  private def processValue(i: Int): Boolean = {
    if (i < 0 || i > 1) throw new IllegalArgumentException("dfdl:setBits arguments must each be 0 or 1, but value was: %s.".format(i))
    if (i == 0) false
    else true
  }
}
