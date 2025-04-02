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

import java.lang.{ Long => JLong }
import java.lang.{ Number => JNumber }
import java.math.{ BigInteger => JBigInt }

import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.unparsers._

case class ConvertNonBaseTenTextNumberUnparser(
  override val context: ElementRuntimeData,
  base: Int
) extends TextPrimUnparser {

  override def runtimeDependencies = Vector()

  override def unparse(state: UState): Unit = {

    val node = state.currentInfosetNode.asSimple
    val value = node.dataValue

    val baseStr = value.getNumber match {
      case bi: JBigInt => {
        if (bi.compareTo(JBigInt.ZERO) < 0) {
          UE(
            state,
            "Unable to unparse negative values when dfdl:textStandardBase=\"%d\": %s",
            base,
            bi.toString
          )
        }
        bi.toString(base)
      }
      case n: JNumber => {
        val l = n.longValue
        if (l < 0) {
          UE(
            state,
            "Unable to unparse negative values when dfdl:textStandardBase=\"%d\": %s",
            base,
            l.toString
          )
        }
        base match {
          case 2 => JLong.toBinaryString(l)
          case 8 => JLong.toOctalString(l)
          case 16 => JLong.toHexString(l)
        }
      }
    }

    node.overwriteDataValue(baseStr)
  }
}
