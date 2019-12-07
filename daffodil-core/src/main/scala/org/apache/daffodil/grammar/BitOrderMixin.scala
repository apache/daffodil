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

package org.apache.daffodil.grammar
import org.apache.daffodil.dsom.Term
import org.apache.daffodil.equality.TypeEqual
import org.apache.daffodil.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.processors.CheckByteAndBitOrderEv
import org.apache.daffodil.processors.CheckBitOrderAndCharsetEv
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.dsom.{ Binary, NoText }
import org.apache.daffodil.dsom.ElementBase

trait BitOrderMixin extends GrammarMixin with ByteOrderAnalysisMixin { self: Term =>

  protected final lazy val optDefaultBitOrder: Option[BitOrder] = {
    val bitOrd =
      if (tunable.requireBitOrderProperty) {
        Some(bitOrder)
      } else {
        optionBitOrder
      }
    bitOrd
  }

  final lazy val defaultBitOrder = optDefaultBitOrder.getOrElse(BitOrder.MostSignificantBitFirst)

  /**
   * Conservatively determines if this term is known to have
   * the same bit order as the previous thing.
   *
   * If uncertain, returns false.
   */
  final protected lazy val isKnownSameBitOrder: Boolean = {
    val res =
      if (enclosingTerm.isEmpty) false // root needs bit order
      else {
        val optPrior = this.nearestPriorPhysicalTermSatisfying(_.optDefaultBitOrder.isDefined)
        optPrior match {
          case None => false // no prior that has a bit order we could be the same as
          case Some(prior) => {
            if (prior.defaultBitOrder =:= this.defaultBitOrder) true
            else false
          }
        }
      }
    res
  }

  protected lazy val hasUniformBitOrderThroughout: Boolean = {
    val res = termChildren.map { t => t.isKnownSameBitOrder && t.hasUniformBitOrderThroughout }.forall(x => x)
    res
  }

  protected final lazy val needsBitOrderChange = {
    enclosingTerm.isEmpty || (
      optionBitOrder.isDefined &&
      thereIsAByteOrderDefined && // if there is no byte order, then there's no need for bit order. The two go together. An all-textual format doesn't need either one.
      (!isKnownSameBitOrder ||
        (isArray && !hasUniformBitOrderThroughout)))
  }

  private lazy val maybeByteOrderEv = self match {
    case eb: ElementBase => eb.maybeByteOrderEv
    case _ => Maybe.Nope
  }

  lazy val maybeCheckByteAndBitOrderEv = {
    //
    // TODO: Performance: could be improved, as there are situations where byteOrder
    // is defined, but still we know it will not be used and this could
    // be Nope in those cases also. An example would be a 100% text-only item.
    //
    if (!isRepresented || !optionByteOrderRaw.isDefined)
      Maybe.Nope
    else {
      val checkByteAndBitOrder = {
        val ev = new CheckByteAndBitOrderEv(ci, defaultBitOrder,
          maybeByteOrderEv)
        ev.compile(tunable)
        ev
      }
      Maybe(checkByteAndBitOrder)
    }
  }

  lazy val maybeCheckBitOrderAndCharset = {
    val se = summaryEncoding
    if (!isRepresented || se == NoText || se == Binary)
      Maybe.Nope
    else {
      val checkBitOrderAndCharset = {
        val ev = new CheckBitOrderAndCharsetEv(ci, defaultBitOrder, charsetEv)
        ev.compile(tunable)
        ev
      }
      Maybe(checkBitOrderAndCharset)
    }
  }

}
