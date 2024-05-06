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

import org.apache.daffodil.lib.calendar.DFDLDate
import org.apache.daffodil.lib.calendar.DFDLTime
import org.apache.daffodil.lib.util.Numbers.asInt
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueDate
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueDateTime
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueString
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueTime

case class XSInt(recipe: CompiledDPath) extends RecipeOpWithSubRecipes(recipe) {
  override def run(dstate: DState): Unit = {
    recipe.run(dstate)
    val basicValue = dstate.currentValue
    val value = asInt(basicValue.getAnyRef)
    dstate.setCurrentValue(value)
  }
}

case class XSString(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  override def computeValue(value: DataValuePrimitive, dstate: DState): DataValueString = {
    val res: DataValueString = value.getAnyRef match {
      case hb: Array[Byte] => HexBinaryToString.computeValue(hb, dstate)
      case x => x.toString()
    }
    res
  }
}

case class XSDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  val name = "XSDateTime"

  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueDateTime = {
    val result = a.getAnyRef match {
      case _: DFDLTime =>
        throw new NumberFormatException(
          "Casting from xs:time to xs:dateTime can never succeed."
        )
      case _ => StringToDateTime.computeValue(a, dstate)
    }
    result
  }
}

case class XSDate(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  val name = "XSDate"

  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueDate = {
    val result = a.getAnyRef match {
      case _: DFDLTime =>
        throw new NumberFormatException("Casting from xs:time to xs:date can never succeed.")
      case _ => StringToDate.computeValue(a, dstate)
    }
    result
  }
}

case class XSTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  val name = "XSTime"

  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueTime = {
    val result = a.getAnyRef match {
      case _: DFDLDate =>
        throw new NumberFormatException("Casting from xs:date to xs:time can never succeed")
      case _ => StringToTime.computeValue(a, dstate)
    }
    result
  }
}
