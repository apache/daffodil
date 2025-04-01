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

package org.apache.daffodil.unparsers.runtime1

import java.lang.{ Float => JFloat }
import java.lang.{ Number => JNumber }
import java.math.{ BigDecimal => JBigDecimal }

import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Numbers.isZero
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.processors._
import org.apache.daffodil.runtime1.processors.parsers.TextDecimalVirtualPointMixin
import org.apache.daffodil.runtime1.processors.unparsers._

case class ConvertTextCombinatorUnparser(
  rd: TermRuntimeData,
  valueUnparser: Unparser,
  converterUnparser: Unparser
) extends CombinatorUnparser(rd) {

  override def runtimeDependencies = Vector()

  override def childProcessors = Vector(converterUnparser, valueUnparser)

  override def unparse(state: UState): Unit = {
    converterUnparser.unparse1(state)

    if (state.processorStatus ne Success) return

    valueUnparser.unparse1(state)
  }
}

case class ConvertTextNumberUnparser(
  textNumberFormatEv: TextNumberFormatEv,
  zeroRep: Maybe[String],
  override val context: ElementRuntimeData,
  override val textDecimalVirtualPoint: Int
) extends PrimUnparser
  with TextDecimalVirtualPointMixin
  with ToBriefXMLImpl {

  override def runtimeDependencies = Vector(textNumberFormatEv)

  override def unparse(state: UState): Unit = {

    val node = state.currentInfosetNode.asSimple
    val value = node.dataValue.getNumber

    // The type of value should have the type of S, but type erasure makes this
    // difficult to assert. Could probably check this with TypeTags or Manifest
    // if we find this is not the case. Want something akin to:
    // Assert.invariant(value.isInstanceOf[S])

    val df = textNumberFormatEv.evaluate(state)
    val dfs = df.getDecimalFormatSymbols

    val scaledValue = applyTextDecimalVirtualPointForUnparse(value)

    val strRep = scaledValue match {
      //
      // Note that Java bigDecimal equals (which is invoked by the == operator) does NOT
      // consider two bigDecimal values equal if they have different scale.
      // So, if you take the number 0 and scale it, you get a "different zero" as far as equals
      // is concerned.
      //
      // Java BigDecimal is also not equal to any other Number sub-type.
      // Really we should revisit the decision to use Java number types instead of Scala types.
      //
      // You must use compareTo method to compare bigDecimal values.
      //
      case n: JNumber if zeroRep.isDefined && isZero(n) => zeroRep.get
      case _ =>
        try {
          if (
            (context.optPrimType.get eq NodeInfo.Float) &&
            JFloat.isFinite(scaledValue.asInstanceOf[Float])
          ) {
            // ICU4J has has no format() function that handles a float. Instead, ICU4J converts
            // the float to a double, and formats that value. However, formatting a double as a
            // string has different precision than formatting a float as a string, resulting in
            // extra precision that implies more accuracy than actually exists for floats, and
            // can also lead to failures to exactly round. To solve this, we convert the float
            // to a String, convert that String to a BigDecimal, and then format that. This is
            // more expensive, but ensures we unparse the exact same precision as represented by
            // the float.
            df.format(new JBigDecimal(scaledValue.toString))
          } else {
            df.format(scaledValue)
          }
        } catch {
          case e: java.lang.ArithmeticException =>
            UE(state, "Unable to format number to pattern: %s", e.getMessage())
        }
    }

    node.overwriteDataValue(strRep)
  }
}
