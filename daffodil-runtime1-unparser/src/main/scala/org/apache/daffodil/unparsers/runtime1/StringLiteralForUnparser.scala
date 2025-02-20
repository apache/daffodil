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

import org.apache.daffodil.lib.cookers.EntityReplacer
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.runtime1.dsom.DPathCompileInfo
import org.apache.daffodil.runtime1.processors.Evaluatable
import org.apache.daffodil.runtime1.processors.InfosetCachedEvaluatable
import org.apache.daffodil.runtime1.processors.OutputNewLineEv
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState

class NilStringLiteralForUnparserEv(
  tci: DPathCompileInfo,
  maybeOutputNewLineEv: Maybe[OutputNewLineEv],
  stringLiteralRaw: String
) extends Evaluatable[String](tci)
  with InfosetCachedEvaluatable[String] {

  override def runtimeDependencies = maybeOutputNewLineEv.toList

  override protected def compute(state: ParseOrUnparseState): String = {
    val endMarker = "__daffodil_stringLiteralForUnparser_endMarker__"
    Assert.invariant(!stringLiteralRaw.endsWith(endMarker))
    val rawWithEndMark = stringLiteralRaw + endMarker
    EntityReplacer { er =>
      val cookedWithEndMark = er.replaceForUnparse(rawWithEndMark)
      val chunksSeparatedByNL = cookedWithEndMark.split(er.markerForNL).toSeq
      val last = chunksSeparatedByNL.last
      val butLast = chunksSeparatedByNL.take(chunksSeparatedByNL.length - 1)
      val chunks =
        if (last == endMarker) {
          // this means the original string ended with a %NL;
          // because the endMarker is all by itself.
          butLast :+ "" // replace it by an empty string
        } else {
          // the original string did not end with a %NL;
          // so that means whatever the last token is, it has our endMarker
          // glued onto it, and we must remove that.
          Assert.invariant(last.endsWith(endMarker))
          butLast :+ last.replace(endMarker, "")
        }

      if (chunks.length == 1) {
        // there are no NL entities. There is only a single chunk.
        chunks.head
      } else {
        tci.schemaDefinitionUnless(
          maybeOutputNewLineEv.isDefined,
          "Property dfdl:outputNewLine is required, but it is not defined."
        )
        val nl = maybeOutputNewLineEv.get.evaluate(state)
        val sl = chunks.mkString(nl)
        sl
      }
    }
  }
}
