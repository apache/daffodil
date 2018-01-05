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

package org.apache.daffodil.dpath

import org.apache.daffodil.util.Numbers._
import org.apache.daffodil.calendar.DFDLDate
import org.apache.daffodil.calendar.DFDLTime

case class XSInt(recipe: CompiledDPath) extends RecipeOpWithSubRecipes(recipe) {
  override def run(dstate: DState) {
    recipe.run(dstate)
    val basicValue = dstate.currentValue
    val value = asInt(basicValue)
    dstate.setCurrentValue(value)
  }
}

case class XSString(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(value: AnyRef, dstate: DState) = {
    val res = value match {
      case hb: Array[Byte] => HexBinaryToString.computeValue(hb, dstate)
      case _ =>
        value.toString
    }
    res
  }
}

case class XSDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  val name = "XSDateTime"

  override def computeValue(a: AnyRef, dstate: DState): AnyRef = {
    val result = a match {
      case _: DFDLTime => throw new NumberFormatException("Casting from xs:time to xs:dateTime can never succeed.")
      case _ => StringToDateTime.computeValue(a, dstate)
    }
    result
  }
}

case class XSDate(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  val name = "XSDate"

  override def computeValue(a: AnyRef, dstate: DState): AnyRef = {
    val result = a match {
      case _: DFDLTime => throw new NumberFormatException("Casting from xs:time to xs:date can never succeed.")
      case _ => StringToDate.computeValue(a, dstate)
    }
    result
  }
}

case class XSTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  val name = "XSTime"

  override def computeValue(a: AnyRef, dstate: DState): AnyRef = {
    val result = a match {
      case _: DFDLDate => throw new NumberFormatException("Casting from xs:date to xs:time can never succeed")
      case _ => StringToTime.computeValue(a, dstate)
    }
    result
  }
}
