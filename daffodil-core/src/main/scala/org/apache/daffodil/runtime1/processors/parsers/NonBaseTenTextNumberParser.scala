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

package org.apache.daffodil.runtime1.processors.parsers

import java.math.{ BigInteger => JBigInt }

import org.apache.daffodil.runtime1.dpath.InvalidPrimitiveDataException
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.processors.ElementRuntimeData

class ConvertNonBaseTenTextNumberParser(override val context: ElementRuntimeData, base: Int)
  extends TextPrimParser {

  override def runtimeDependencies = Vector()

  private val primNumeric = context.optPrimType.get.asInstanceOf[NodeInfo.PrimType.PrimNumeric]

  final def parse(state: PState): Unit = {

    val node = state.simpleElement
    val baseStr = node.dataValueAsString

    if (baseStr == "") {
      PE(state, "Unable to parse %s from empty string", context.optPrimType.get.globalQName)
      return
    }

    // Must explicitly check for and error on text that start with + or -
    // because DFDL does not allow a leading sign character, but parseInt and
    // friends will accept them. Note that textNumberPattern is not used when
    // textStandardBase is not 10--one might otherwise think that a +/- in the
    // pattern would allow this.
    val firstChar = baseStr(0)
    if (firstChar == '-' || firstChar == '+') {
      PE(
        state,
        "Unable to parse %s from base-%d text with leading sign: %s",
        context.optPrimType.get.globalQName,
        base,
        baseStr
      )
      return
    }

    // always parse the base string a BigInt since it allows us to differentiate
    // between invalid characters or just too many characters for the prim type
    val bi =
      try {
        new JBigInt(baseStr, base)
      } catch {
        case e: NumberFormatException =>
          PE(
            state,
            "Unable to parse %s from base-%d text due to invalid characters: %s",
            context.optPrimType.get.globalQName,
            base,
            baseStr
          )
          return
      }

    val num =
      try {
        primNumeric.fromNumber(bi)
      } catch {
        case e: InvalidPrimitiveDataException => {
          PE(state, "%s", e.getMessage)
          return
        }
      }
    node.overwriteDataValue(num)
  }
}
