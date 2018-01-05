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

package org.apache.daffodil.processors.unparsers

import org.apache.daffodil.processors._
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.cookers.EntityReplacer
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.parsers.NumberFormatFactoryBase
import org.apache.daffodil.processors.parsers.ConvertTextNumberParserUnparserHelperBase

case class ConvertTextCombinatorUnparser(
  rd: TermRuntimeData,
  valueUnparser: Unparser,
  converterUnparser: Unparser)
  extends CombinatorUnparser(rd) {

  override lazy val runtimeDependencies = Nil

  override lazy val childProcessors = Seq(converterUnparser, valueUnparser)

  override def unparse(state: UState): Unit = {
    converterUnparser.unparse1(state)

    if (state.processorStatus ne Success) return

    valueUnparser.unparse1(state)
  }
}

case class ConvertTextNumberUnparser[S](
  helper: ConvertTextNumberParserUnparserHelperBase[S],
  nff: NumberFormatFactoryBase[S],
  override val context: ElementRuntimeData)
  extends PrimUnparser
  with ToBriefXMLImpl {

  override lazy val runtimeDependencies = Nil

  override def toString = "to(xs:" + helper.xsdType + ")"
  override lazy val childProcessors = Nil

  lazy val zeroRep: Maybe[String] = helper.zeroRepListRaw.headOption.map { zr =>
    EntityReplacer { _.replaceForUnparse(zr) }
  }

  override def unparse(state: UState): Unit = {

    val node = state.currentInfosetNode.asSimple
    val value = node.dataValue

    // The type of value should have the type of S, but type erasure makes this
    // difficult to assert. Could probably check this with TypeTags or Manifest
    // if we find this is not the case. Want something akin to:
    // Assert.invariant(value.isInstanceOf[S])

    val strRep =
      if (value == 0 && zeroRep.isDefined) {
        zeroRep.get
      } else {
        // Needed because the DecimalFormat class of ICU will call
        // doubleValue on scala's BigInt and BigDecimal because it
        // doesn't recognize it as Java's BigInteger and BigDecimal.
        // This caused large numbers to be truncated silently.
        value match {
          case bd: scala.math.BigDecimal => Assert.usageError("Received scala.math.BigDecimal, expected java.math.BigDecimal.")
          case bi: scala.math.BigInt => Assert.usageError("Received scala.math.BigInt, expected java.math.BigInteger.")
          case _ => // OK
        }
        val df = nff.getNumFormat(state)
        df.get.format(value)
      }

    node.overwriteDataValue(strRep)
  }
}
